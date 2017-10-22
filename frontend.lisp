#|
 This file is a part of Events
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.events)

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
    (setf (dm:field event "repeat") (case (dm:field event "interval")
                                      (0 "Once")
                                      (1 "Daily")
                                      (2 "Weekly")
                                      (3 "Monthly")
                                      (4 "Yearly")
                                      (T "???")))
    (r-clip:process T :event event)))

(define-page edit "events/([^/]+)/edit" (:uri-groups (id) :clip "edit.ctml")
  (let ((event (ensure-event id)))
    (check-permission '(edit delete) event)
    (r-clip:process T :event event
                      :error (get-var "error")
                      :message (get-var "message"))))
