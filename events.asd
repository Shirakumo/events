(asdf:defsystem #:events
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :version "1.0.0"
  :description "An event planning system for Radiance."
  :homepage "https://Shirakumo.github.io/events/"
  :bug-tracker "https://github.com/Shirakumo/events/issues"
  :source-control (:git "https://github.com/Shirakumo/events.git")
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
               :com.inuoe.jzon
               :cl-ppcre
               :r-data-model
               :r-clip
               :3bmd
               :local-time
               :babel
               :trivial-mimes
               :iclendar))
