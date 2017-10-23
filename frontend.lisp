#|
 This file is a part of Events
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.events)

(defun maybe-update-event (event start offset)
  (when (and (< start (get-universal-time))
             (< 0 (dm:field event "interval")))
    (loop while (< start (get-universal-time))
          do (setf start (apply-interval start (dm:field event "interval"))))
    (v:info :test "~a ~a" start (iso-stamp (+ start offset)))
    (setf (dm:field event "start") (iso-stamp (+ start offset)))
    (dm:save event)))

(define-page create "events/" (:clip "edit.ctml")
  (let ((event (dm:hull 'events)))
    (check-permission 'new)
    (setf (dm:field event "duration") 60)
    (setf (dm:field event "interval") 0)
    (r-clip:process T :event event
                      :error (get-var "error")
                      :message (get-var "message"))))

(define-page view "events/([^/]+)" (:uri-groups (id) :clip "view.ctml")
  (let ((event (ensure-event id)))
    (check-permission 'view event)
    (multiple-value-bind (start offset) (event-start-stamp event)
      (maybe-update-event event start offset)
      (setf (dm:field event "begin") start)
      (setf (dm:field event "end") (+ start (* 60 (dm:field event "duration"))))
      (setf (dm:field event "repeat") (case (dm:field event "interval")
                                        (0 "Once")
                                        (1 "Daily")
                                        (2 "Weekly")
                                        (3 "Monthly")
                                        (4 "Yearly")
                                        (T "???")))
      (setf (dm:field event "description") (markdown.cl:parse (dm:field event "description")))
      (r-clip:process T :event event
                        :error (get-var "error")
                        :message (get-var "message")))))

(define-page edit "events/([^/]+)/edit" (:uri-groups (id) :clip "edit.ctml")
  (let ((event (ensure-event id)))
    (check-permission '(edit delete) event)
    (r-clip:process T :event event
                      :error (get-var "error")
                      :message (get-var "message"))))
