#|
 This file is a part of Events
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.events)

(defun ->ical-date-time (timestamp)
  (multiple-value-bind (s m h dd mm yy) (decode-universal-time timestamp 0)
    (iclendar:make-date-time :year yy :month mm :date dd :hour h :minute m :second s :utc-p T)))

(defun render-ical (event)
  (dm:with-model-fields event (_id time author duration interval start-stamp)
    (let* ((domain (first (mconfig :radiance :domains)))
           (event (make-instance 'iclendar:event :uid (format NIL "shirakumo/events:~a/~d" domain _id)
                                                 :stamp (->ical-date-time time)
                                                 :start (->ical-date-time start-stamp)
                                                 :organizer (uri-to-url (make-uri :domains '("user") :path (user:username author))
                                                                        :representation :external)
                                                 :end (->ical-date-time (+ start-stamp (* 60 duration)))
                                                 :summary (event-pure-description event)))
           (calendar (make-instance 'iclendar:calendar :product (format NIL "-//shirakumo//NONSGML events/~a/EN" domain)
                                                       :components (list event))))
      (when (< 0 interval)
        (setf (iclendar:recurrence-rule event) (iclendar:make-recurrence (ecase interval (1 :daily) (2 :weekly) (3 :monthly) (4 :yearly)))))
      (iclendar:serialize calendar NIL))))

(define-page ical "events/([^/]+)/ical" (:uri-groups (id))
  (let ((event (ensure-event id)))
    (check-permission 'view event)
    (setf (content-type *response*) "text/calendar")
    (setf (header "Content-Disposition")
          (format NIL "attachment; filename=~s" (format NIL "~a.ics" (dm:field event "title"))))
    (maybe-update-event-start event)
    (render-ical event)))
