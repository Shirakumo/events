(in-package #:org.shirakumo.radiance.events)

(defvar *flavor-dir* (environment-module-pathname #.*package* :data "flavor/"))
(defvar *allowed-images* #("image/jpeg" "image/png" "image/gif" "image/x-ms-bmp" "image/svg+xml"))

(defun api-event-output (event)
  (cond ((string= "true" (post/get "browser"))
         (redirect (event-url event)))
        (T
         (let ((table (dm:fields event)))
           (setf (dm:field table "description")
                 (rendered-event-description event))
           (when (typep (dm:field table "description") 'plump:node)
             (setf (dm:field table "description")
                   (plump:serialize (dm:field table "description") NIL)))
           (setf (dm:field table "status")
                 (string-downcase (->status (dm:field table "status"))))
           (api-output table)))))

(defun handle-file (file event)
  (let ((mime (mimes:mime-lookup (second file))))
    (if mime
        (unless (find mime *allowed-images* :test #'string-equal)
          (error "Files of type ~s are not allowed." mime))
        (error "Unknown file format."))
    (when (and (integerp (config :file :size-limit))
               (<= (config :file :size-limit)
                   (/ (file-size (first file)) 1024 1024)))
      (error "File is too big. Must be below ~aMb" (config :file :size-limit)))
    (let* ((name (format NIL "~a-~a.~a"
                         (dm:id event) (get-universal-time) (mimes:mime-file-type mime)))
           (path (merge-pathnames name *flavor-dir*)))
      (ensure-directories-exist path)
      (uiop:copy-file (first file) path)
      name)))

(define-api events/view (id) ()
  (let ((event (ensure-event id)))
    (check-permission 'view event)
    (api-output-event event)))

(define-api events/create (title location start &optional description duration interval link flavor) ()
  (check-permission 'new)
  (when (< (parse-iso-stamp start) (- (get-universal-time)
                                      (* 60 60 12)))
    (error "The start date must be in the future."))
  (db:with-transaction ()
    (let ((event (create-event title location start
                               :link link
                               :author (auth:current)
                               :description description
                               :duration (when duration (parse-integer duration))
                               :interval (when interval (parse-integer interval)))))
      (when flavor
        (setf (dm:field event "flavor") (handle-file flavor event))
        (dm:save event))
      (api-event-output event))))

(define-api events/edit (id &optional title location start description duration interval link flavor) ()
  (let ((event (ensure-event id)))
    (check-permission 'edit event)
    (when (and start (< (parse-iso-stamp start) (- (get-universal-time)
                                                   (* 60 60 12))))
      (error "The start date must be in the future."))
    (when flavor
      (setf flavor (handle-file flavor event))
      (when (string/= "" (dm:field event "flavor"))
        (uiop:delete-file-if-exists (merge-pathnames (dm:field event "flavor") *flavor-dir*))))
    (edit-event event :title title
                      :location location
                      :start start
                      :link link
                      :flavor flavor
                      :description description
                      :duration (when duration (parse-integer duration))
                      :interval (when interval (parse-integer interval)))
    (api-event-output event)))

(define-api events/cancel (id &optional (cancel-action "cancel")) ()
  (let ((event (ensure-event id)))
    (check-permission 'edit event)
    (when (< (dm:field event "status") 2)
      (edit-event event :status (if (string-equal cancel-action "cancel") 1 0)))
    (api-event-output event)))

(define-api events/delete (id) ()
  (let ((event (ensure-event id)))
    (check-permission 'delete event)
    (delete-event event)
    (if (string= "true" (post/get "browser"))
        (redirect (uri-to-url "events/list" :representation :external
                                            :query '(("message" . "Event deleted."))))
        (api-output `(("_id" . ,(dm:id event)))))))
