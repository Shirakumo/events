#|
 This file is a part of Events
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem #:events
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :version "1.0.0"
  :description "An event planning system for Radiance."
  :homepage "https://github.com/Shirakumo/events"
  :components ((:file "module")
               (:file "timedata")
               (:file "objects")
               (:file "frontend")
               (:file "ical")
               (:file "api"))
  :depends-on ((:interface :database)
               (:interface :user)
               (:interface :auth)
               (:interface :cache)
               :drakma
               :yason
               :cl-ppcre
               :r-data-model
               :r-clip
               :3bmd
               :local-time
               :babel
               :trivial-mimes))
