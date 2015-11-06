(in-package :hu.dwim.mosquitto)

(export
 (let ((*package* (find-package :hu.dwim.mosquitto)))
   '(
     ;;   #:errno
     ;;   #:strerror
     ;;   #:version

     c-fun/not-null

     lib-init
     lib-cleanup
     lib-version
     mosquitto
     connect
     destroy

     publish

     new-session
     current-session
     with-new-session
     with-session

     mosquitto-error
     process-some-events
     ))
 :hu.dwim.mosquitto)

;; define some convenience type aliases
(autowrap:define-foreign-alias 'mosquitto '(:struct (mosquitto)))
