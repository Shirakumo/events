#|
 This file is a part of Events
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.events)

(define-hook event-updated (event))
(define-hook event-created (event))
(define-hook event-deleted (event))

(define-trigger db:connected ()
  (db:create 'events '((title (:varchar 32))
                       (time (:integer 5))
                       (author (:varchar 32))
                       (link (:varchar 64))
                       (flavor (:varchar 64))
                       (description :text)
                       (location (:varchar 32))
                       (start (:varchar 16))
                       (start-stamp (:integer 5))
                       (duration :integer)
                       (interval :integer)
                       (status (:integer 1)))))

(define-trigger user:ready ()
  (defaulted-config 2 :file :size-limit)
  
  (defaulted-config (list
                     (perm events new)
                     (perm events view)
                     (perm events edit own)
                     (perm events delete own))
                    :permissions :default)

  (defaulted-config (list
                     (perm events view))
                    :permissions :anonymous)

  (apply #'user:add-default-permissions (config :permissions :default))
  (apply #'user:grant "anonymous" (config :permissions :anonymous)))

(define-version-migration events (NIL 1.0.0)
  (let ((previous (make-pathname :name NIL
                                 :type NIL
                                 :defaults (merge-pathnames "flavor/" (mconfig-pathname #.*package*)))))
    (when (uiop:directory-exists-p previous)
      (ensure-directories-exist (environment-module-directory #.*package* :data))
      (rename-file previous *flavor-dir*)))
  (let ((anonymous (user:get "anonymous")))
    (dolist (user (user:list))
      (unless (eql user anonymous)
        (apply #'user:grant user (config :permissions :default))))))

(defun ->status (thing)
  (etypecase thing
    ((eql 0) :active)
    ((eql 1) :cancelled)
    ((eql 2) :ended)
    ((or symbol string)
     (cond ((string-equal thing "active") :active)
           ((string-equal thing "cancelled") :cancelled)
           ((string-equal thing "ended") :ended)))))

(defun status->int (status)
  (ecase (->status status)
    (:active 0)
    (:cancelled 1)
    (:ended 2)))

(defun ensure-event (event-ish)
  (etypecase event-ish
    (dm:data-model event-ish)
    (string (ensure-event
             (or (ignore-errors (db:ensure-id event-ish))
                 (error 'request-not-found :message (format NIL "No event with ID ~s was found." event-ish)))))
    (integer (or (dm:get-one 'events (db:query (:= '_id event-ish)))
                 (error 'request-not-found :message (format NIL "No event with ID ~a was found." event-ish))))))

(defun create-event (title location start &key author link description duration interval flavor)
  (let ((event (dm:hull 'events))
        (author (etypecase author
                  (string author)
                  (user:user (user:username author)))))
    (db:with-transaction ()
      (setf (dm:field event "title") title
            (dm:field event "time") (get-universal-time)
            (dm:field event "author") author
            (dm:field event "link") (or link "")
            (dm:field event "flavor") (or flavor "")
            (dm:field event "description") (or description "")
            (dm:field event "location") location
            (dm:field event "start") start
            (dm:field event "start-stamp") (event-start-stamp event)
            (dm:field event "duration") (or duration (* 60 60))
            (dm:field event "interval") (or interval 0)
            (dm:field event "status") 0)
      (dm:insert event))
    (trigger 'event-created event)
    event))

(defun delete-event (event)
  (let ((event (ensure-event event)))
    (db:with-transaction ()
      (dm:delete event))
    (trigger 'event-deleted event)
    event))

(defun edit-event (event &key title location start description duration interval link flavor status)
  (let ((event (ensure-event event)))
    (db:with-transaction ()
      (when title
        (setf (dm:field event "title") title))
      (when location
        (setf (dm:field event "location") location))
      (when start
        (setf (dm:field event "start") start))
      (when (or location start)
        (setf (dm:field event "start-stamp") (event-start-stamp event)))
      (when link
        (setf (dm:field event "link") link))
      (when flavor
        (setf (dm:field event "flavor") flavor))
      (when description
        (setf (dm:field event "description") description))
      (when duration
        (setf (dm:field event "duration") duration))
      (when interval
        (setf (dm:field event "interval") interval))
      (when status
        (setf (dm:field event "status") (status->int status)))
      ;; If we're ended and the start date is now in the future, un-end.
      (when (and (= 2 (dm:field event "status"))
                 (< (get-universal-time) (dm:field event "start-stamp")))
        (setf (dm:field event "status") 0))
      (dm:save event))
    (trigger 'event-updated event)
    event))

(defun event-url (event)
  (make-url :domains '("events")
            :path (princ-to-string (dm:id (ensure-event event)))))

(defun parse-iso-stamp (stamp)
  (cl-ppcre:register-groups-bind (y m d hh mm NIL ss) ("(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2})(:(\\d{2}))?Z?" stamp)
    (encode-universal-time (if ss (parse-integer ss) 0)
                           (parse-integer mm)
                           (parse-integer hh)
                           (parse-integer d)
                           (parse-integer m)
                           (parse-integer y)
                           0)))

(defun iso-stamp (stamp)
  (multiple-value-bind (ss mm hh d m y)
      (decode-universal-time stamp 0)
    (declare (ignore ss))
    (format NIL "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d"
            y m d hh mm)))

(defun event-countdown (event)
  (if (= 0 (dm:field event "status"))
      (let* ((remaining (max 0 (- (dm:field event "start-stamp") (get-universal-time))))
             (s (mod (floor (/ remaining 1)) 60))
             (m (mod (floor (/ remaining 60)) 60))
             (h (mod (floor (/ remaining 60 60)) 24))
             (d (mod (floor (/ remaining 60 60 24)) 365))
             (y (floor (/ remaining 60 60 24 365))))
        (with-output-to-string (out)
          (unless (= 0 y) (format out "~dy " y))
          (unless (= 0 y d) (format out "~dd " d))
          (unless (= 0 y d h) (format out "~d:" h))
          (unless (= 0 y d h m) (format out "~2,'0d:" m))
          (unless (= 0 y d h m s) (format out "~2,'0d" s))))
      ""))

(defun event-start-stamp (event)
  (let* ((event (ensure-event event))
         (start-stamp (parse-iso-stamp (dm:field event "start")))
         (offset (timezone-offset (dm:field event "location") (universal-to-unix-time start-stamp))))
    (values (- start-stamp offset)
            offset)))

(defun permitted-p (action &optional event (user (or (auth:current) (user:get "anonymous"))))
  (if (listp action)
      (loop for a in action thereis (permitted-p a event user))
      (or (and event
               (equal (dm:field event "author") (user:username user))
               (user:check user `(events ,action own)))
          (user:check user `(events ,action)))))

(defun check-permission (action &optional event (user (or (auth:current) (user:get "anonymous"))))
  (unless (permitted-p action event user)
    (error 'request-denied :message (format NIL "You do not have the permission to ~a events."
                                            action))))

(defun interval->label (interval)
  (case interval
    (0 "Once")
    (1 "Daily")
    (2 "Weekly")
    (3 "Monthly")
    (4 "Yearly")
    (T NIL)))

(defun apply-interval (stamp interval)
  (let ((stamp (local-time:universal-to-timestamp stamp)))
    (ecase interval
      (0 (local-time:timestamp-to-universal stamp))
      (1 (local-time:timestamp-to-universal
          (local-time:timestamp+ stamp 1 :day local-time:+utc-zone+)))
      (2 (local-time:timestamp-to-universal
          (local-time:timestamp+ stamp 7 :day local-time:+utc-zone+)))
      (3 (local-time:timestamp-to-universal
          (local-time:timestamp+ stamp 1 :month local-time:+utc-zone+)))
      (4 (local-time:timestamp-to-universal
          (local-time:timestamp+ stamp 1 :year local-time:+utc-zone+))))))

(defun maybe-update-event-start (event)
  (let* ((event (ensure-event event))
         (interval (dm:field event "interval")))
    (when (event-out-of-date-p event)
      (multiple-value-bind (start offset) (event-start-stamp event)
        (cond ((< 0 interval)
               (loop while (< start (get-universal-time))
                     do (setf start (apply-interval start interval)))
               (db:with-transaction ()
                 (setf (dm:field event "start") (iso-stamp (+ start offset)))
                 (setf (dm:field event "start-stamp") start)
                 (dm:save event))
               (trigger 'event-updated event))
              ((= 0 (dm:field event "status"))
               (db:with-transaction ()
                 (setf (dm:field event "status") 2)
                 (dm:save event))
               (trigger 'event-updated event)))))
    event))

(defun event-out-of-date-p (event)
  (let ((event (ensure-event event)))
    (and (<= (dm:field event "start-stamp") (get-universal-time))
         (= 0 (dm:field event "status")))))

(defun process-hidden-blocks (text &optional remove)
  (cl-ppcre:regex-replace-all
   "\\[\\? *(.*?) *\\?\\]"
   text
   (lambda (text s e ms me rs re)
     (declare (ignore s e ms me))
     (if remove
         ""
         (subseq text (aref rs 0) (aref re 0))))))

(defun rendered-event-description (event)
  (let ((value (cache:with-cache (event-description (dm:id event)) NIL
                 (with-output-to-string (o)
                   (3bmd:parse-string-and-print-to-stream
                    (process-hidden-blocks
                     (dm:field event "description")
                     (< (get-universal-time) (dm:field event "start-stamp")))
                    o)))))
    (typecase value
      (string value)
      (vector (babel:octets-to-string value))
      (plump:node value))))

(defun event-pure-description (event)
  (with-output-to-string (out)
    (3bmd:parse-string-and-print-to-stream
     (process-hidden-blocks
      (dm:field event "description")
      (< (get-universal-time) (dm:field event "start-stamp")))
     out :format :plain)))

(defun event-short-description (event)
  (with-output-to-string (out)
    (with-input-from-string (in (event-pure-description event))
      (loop while (find (peek-char NIL in NIL) #(#\Return #\Linefeed))
            do (read-char in))
      (loop repeat 140
            for c = (read-char in NIL)
            until (or (null c) (find c #(#\Return #\Linefeed)))
            do (write-char c out)))))

(define-trigger (event-updated renew-description) (event)
  (cache:renew 'event-description (dm:id event)))

(define-trigger (event-deleted delete-description) (event)
  (cache:renew 'event-description (dm:id event)))
