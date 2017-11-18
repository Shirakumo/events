#|
 This file is a part of Events
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.events)

(defun api-event-output (event)
  (cond ((string= "true" (post/get "browser"))
         (redirect (event-url event)))
        (T
         (api-output (dm:fields event)))))

(define-api events/view (id) ()
  (let ((event (ensure-event id)))
    (check-permission 'view event)
    (api-output event)))

(define-api events/create (title location start &optional description duration interval link) ()
  (check-permission 'new)
  (when (< (parse-iso-stamp start) (- (get-universal-time)
                                      (* 60 60 12)))
    (error "The start date must be in the future."))
  (let ((event (create-event title location start
                             :link link
                             :author (auth:current)
                             :description description
                             :duration (when duration (parse-integer duration))
                             :interval (when interval (parse-integer interval)))))
    (api-event-output event)))

(define-api events/edit (id &optional title location start description duration interval link) ()
  (let ((event (ensure-event id)))
    (check-permission 'edit event)
    (when (and start (< (parse-iso-stamp start) (- (get-universal-time)
                                                   (* 60 60 12))))
      (error "The start date must be in the future."))
    (edit-event event :title title
                      :location location
                      :start start
                      :link link
                      :description description
                      :duration (when duration (parse-integer duration))
                      :interval (when interval (parse-integer interval)))
    (api-event-output event)))

(define-api events/cancel (id &optional (cancel-action "cancel")) ()
  (let ((event (ensure-event id)))
    (check-permission 'edit event)
    (edit-event event :cancelled (if (string-equal cancel-action "cancel") T NIL))
    (api-event-output event)))

(define-api events/delete (id) ()
  (let ((event (ensure-event id)))
    (check-permission 'delete event)
    (delete-event event)
    (if (string= "true" (post/get "browser"))
        (redirect (uri-to-url "events/" :representation :external
                                        :query '(("message" . "Event deleted."))))
        (api-output `(("_id" . ,(dm:id event)))))))
