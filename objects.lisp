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
                       (cancelled (:integer 1)))))

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
            (dm:field event "cancelled") 0)
      (dm:insert event))
    (trigger 'event-created event)
    event))

(defun delete-event (event)
  (let ((event (ensure-event event)))
    (db:with-transaction ()
      (dm:delete event))
    (trigger 'event-deleted event)
    event))

(defun edit-event (event &key title location start description duration interval link flavor (cancelled NIL cancelled-p))
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
      (when cancelled-p
        (setf (dm:field event "cancelled") (if cancelled 1 0)))
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
  (let ((event (ensure-event event)))
    (when (event-out-of-date-p event)
      (multiple-value-bind (start offset) (event-start-stamp event)
        (loop while (< start (get-universal-time))
              do (setf start (apply-interval start (dm:field event "interval"))))
        (db:with-transaction ()
          (setf (dm:field event "start") (iso-stamp (+ start offset)))
          (setf (dm:field event "start-stamp") start)
          (dm:save event))
        (trigger 'event-updated event)))
    event))

(defun event-out-of-date-p (event)
  (let ((event (ensure-event event)))
    (and (< (dm:field event "start-stamp") (get-universal-time))
         (< 0 (dm:field event "interval")))))

(defun rendered-event-description (event)
  (let ((value (cache:with-cache (event-description (dm:id event)) NIL
                 (with-output-to-string (o)
                   (3bmd:parse-string-and-print-to-stream
                    (dm:field event "description") o)))))
    (typecase value
      (string value)
      (vector (babel:octets-to-string value))
      (plump:node value))))

(define-trigger (event-updated renew-description) (event)
  (cache:renew 'event-description (dm:id event)))

(define-trigger (event-deleted delete-description) (event)
  (cache:renew 'event-description (dm:id event)))
