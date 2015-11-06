(defsystem :hu.dwim.mosquitto
  :description "Common Lisp FFI wrapper for libmosquitto, which is an MQTT implementation."
  :author "Attila Lendvai"
  :license "BSD or Bugroff"
  :version "0.1"
  :depends-on (:alexandria
               :cl-autowrap
               :cl-plus-c
               :trivial-garbage)
  :components ((:module "source"
                :serial t
                :components ((:file "autowrap")
                             (:file "package")
                             (:file "package-late")
                             (:file "mosquitto")))
               (:module "autospec"
                :components ((:static-file "mosquitto-spec.h")))))
