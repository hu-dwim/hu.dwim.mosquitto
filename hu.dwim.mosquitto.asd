(defsystem :hu.dwim.mosquitto
  :description "Common Lisp FFI wrapper for libmosquitto, which is an MQTT implementation."
  :author "Attila Lendvai"
  :license "BSD or Bugroff"
  :version "0.8"

  :defsystem-depends-on (:cffi/c2ffi)
  :depends-on (:alexandria
               :cl-autowrap
               :cl-plus-c
               :trivial-garbage)
  :components ((:file "package-stage-1"
                :pathname "source/package-stage-1")
               (:file "ffi-prelude"
                :pathname "source/ffi-prelude"
                :depends-on ("package-stage-1"))
               (:module "source"
                :depends-on ("c2ffi-spec" "package-stage-1")
                :serial t
                :components ((:file "package-stage-2")
                             (:file "package-stage-3")
                             (:file "mosquitto")))
               (:module "c2ffi-spec"
                :depends-on ("ffi-prelude")
                :components ((:cffi/c2ffi-file "mosquitto.h"
                              :package #:hu.dwim.mosquitto.ffi
                              :ffi-name-transformer "hu.dwim.mosquitto::ffi-name-transformer"
                              ;; :ffi-type-transformer "hu.dwim.mosquitto::ffi-type-transformer"
                              :foreign-library-name "hu.dwim.mosquitto.ffi::libmosquitto"
                              :foreign-library-spec ((t (:or "libmosquitto" "libmosquitto.so" "libmosquitto.so.1")))

                              :include-sources ("bits/types.h"
                                                "sys/types.h"
                                                "stdint.h"
                                                "mosquitto.h")
                              :exclude-sources :all
                              :include-definitions ()
                              :exclude-definitions ())))))
