(in-package #:org.shirakumo.radiance.events)

(define-page list-events "events/^$|(?:list(?:/([^/]+))?)" (:uri-groups (page) :clip "list.ctml")
  (check-permission 'view)
  (let* ((page (if page (parse-integer page :junk-allowed T) 0))
         (events (dm:get 'events (db:query :all) :sort '(("time" :desc))
                                                 :amount 20
                                                 :skip (* 20 page))))
    (mapc #'maybe-update-event-start events)
    (r-clip:process T :events events
                      :error (get-var "error")
                      :message (get-var "message"))))

(define-page create ("events/new" 1) (:clip "edit.ctml")
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
    (maybe-update-event-start event)
    (r-clip:process T :event event
                      :error (get-var "error")
                      :message (get-var "message"))))

(define-page edit "events/([^/]+)/edit" (:uri-groups (id) :clip "edit.ctml")
  (let ((event (ensure-event id)))
    (check-permission '(edit delete) event)
    (r-clip:process T :event event
                      :error (get-var "error")
                      :message (get-var "message"))))

(define-page flavor ("/static/events/flavor/(.+)" 1001) (:uri-groups (flavor))
  (setf (header "Cache-Control") "public, max-age=31536000")
  (serve-file (merge-pathnames flavor *flavor-dir*)))
