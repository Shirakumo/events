(in-package #:org.shirakumo.radiance.events)

(defparameter *timezone-api* "https://maps.googleapis.com/maps/api/timezone/json")
(defparameter *geocode-api* "https://maps.googleapis.com/maps/api/geocode/json")

(defun api-request (url parameters &optional (key (config :google-api-key)))
  (let ((drakma:*text-content-types* (list* (cons "application" "json")
                                            drakma:*text-content-types*))
        (drakma:*drakma-default-external-format* :utf-8))
    (com.inuoe.jzon:parse (drakma:http-request url :parameters (list* (cons "key" key) parameters)))))

(defun json-v (data &rest keys)
  (if (null keys)
      data
      (let ((key (first keys)))
        (apply #'json-v
               (etypecase key
                 (integer (elt data key))
                 (string (gethash key data)))
               (rest keys)))))

(defun timezone-data (location &optional (time (get-unix-time)))
  (let* ((data (api-request *timezone-api* `(("sensor" . "false")
                                             ("timestamp" . ,(princ-to-string time))
                                             ("location" . ,(format NIL "~f,~f" (first location) (second location))))))
         (status (json-v data "status")))
    (cond ((string-equal status "ok")
           data)
          ((string-equal status "zero_results")
           (values))
          ((string-equal status "over_query_limit")
           (error "Exceeded allowed amount of queries against the Google Maps API."))
          (T
           (error "Google Maps failed to perform your request for an unknown reason.")))))

(defun geo-data (location)
  (let* ((data (api-request *geocode-api* `(("sensor" . "false")
                                            ("address" . ,location))))
         (status (json-v data "status")))
    (cond ((string-equal status "ok")
           (json-v data "results" 0))
          ((string-equal status "zero_results")
           (error "No location called ~s could be found." location))
          ((string-equal status "over_query_limit")
           (error "Exceeded allowed amount of queries against the Google Maps API."))
          (T
           (error "Google Maps failed to perform your request for an unknown reason.")))))

(defun coordinates (location)
  (restart-case
      (let ((data (geo-data location)))
        (values (list (json-v data "geometry" "location" "lat")
                      (json-v data "geometry" "location" "lng"))
                (json-v data "address_components" 0 "long_name")))
    (use-value (lat/lng)
      :report "Specify latitute and longitude to use."
      :interactive (lambda () (list (read *query-io*)))
      (values lat/lng location))))

(defun timezone-offset (location &optional (time (get-unix-time)))
  (restart-case
      (let ((data (timezone-data (coordinates location) time)))
        (values (+ (json-v data "rawOffset")
                   (json-v data "dstOffset"))))
    (use-value (offset)
      :report "Specify the offset to use."
      :interactive (lambda () (list (read *query-io*)))
      offset)))
