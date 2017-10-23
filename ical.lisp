#|
 This file is a part of Events
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.events)

(defun ical-time (universal-time)
  (multiple-value-bind (ss mm hh d m y)
      (decode-universal-time universal-time 0)
    (format NIL "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0dZ"
            y m d hh mm ss)))

(defun ical-description (description)
  (with-output-to-string (out)
    (loop for c across description
          do (case c
               (#\Return
                (format out "\\n"))
               (#\Linefeed
                (format out "~% "))
               (T (write-char c out))))))

(define-page ical "events/([^/]+)/ical" (:uri-groups (id))
  (let ((event (ensure-event id)))
    (check-permission 'view event)
    (setf (content-type *response*) "text/calendar")
    (setf (header "Content-Disposition")
          (format NIL "attachment; filename=~s" (format NIL "~a.ics" (dm:field event "title"))))
    (dm:with-model-fields event (_id time author duration description interval)
      (let ((start (event-start-stamp event)))
        (format NIL "~
BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//hacksw/handcal//NONSGML v1.0//EN
BEGIN:VEVENT
UID:~a
DTSTAMP:~a
ORGANIZER:~a
DTSTART:~a
DTEND:~a~@[
RRULE:FREQ=~a~]
SUMMARY:~a
END:VEVENT
END:VCALENDAR"
                _id (ical-time time) author (ical-time start) (ical-time (+ start duration))
                (case interval (1 "DAILY") (2 "WEEKLY") (3 "MONTHLY") (4 "YEARLY") (T NIL))
                (ical-description description))))))
