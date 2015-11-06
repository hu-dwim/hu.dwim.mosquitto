(in-package :hu.dwim.mosquitto)

;;;;;;
;;; library loading

;; TODO do we need an asdf:register-image-restore-hook ?

;; TODO FIXME delme
(setf cffi:*foreign-library-directories* '("/home/alendvai/workspace/mosquitto/lib/"))

(cffi:define-foreign-library libmosquitto
  (t (:or "libmosquitto" "libmosquitto.so" "libmosquitto.so.1")))

(cffi:use-foreign-library libmosquitto)

;;;;;;
;;; types

;;;;;;
;;; utils

(defmacro c-fun/not-null (&whole whole fn-name &rest args)
  (with-unique-names (rc)
    `(let ((,rc (c-fun ,fn-name ,@args)))
       (when (null ,rc)
         (error "FFI call unexpectedly returned null. Name: ~S, expression: ~S." ',fn-name ',whole))
       ,rc)))

;;;;;;
;;; conditions

(define-condition mosquitto-error (simple-condition error)
  ((error-code )))

(defun mosquitto-error (datum &rest args)
  (error 'mosquitto-error :format-control datum :format-arguments args))

(defun mosquitto-error* (error-code datum &rest args)
  (apply 'mosquitto-error (concatenate 'string datum (format nil "error code: ~S, ~S." error-code (hu.dwim.mosquitto.ffi:strerror error-code)))
         args))

;;;;;;
;;; API

(defvar *session*)

(defmacro with-new-session ((&key client-id (clean-session? t) user-object
                                  log-callback connect-callback message-callback subscribe-callback
                                  tls-certificate-authority tls-client-certificate tls-private-key password-callback)
                            &body body)
  `(let ((*session* (new-session :client-id ,client-id :clean-session? ,clean-session? :user-object ,user-object
                                 :log-callback ,log-callback :connect-callback ,connect-callback :message-callback ,message-callback :subscribe-callback ,subscribe-callback
                                 :tls-certificate-authority ,tls-certificate-authority :tls-client-certificate ,tls-client-certificate :tls-private-key ,tls-private-key :password-callback ,password-callback)))
     (unwind-protect
          (progn
            ,@body)
       (destroy (the (not null) *session*)))))

(defmacro with-session (session &body body)
  `(let ((*session* ,session))
     ,@body))

(defun current-session ()
  (the (not null) *session*))

(defun %ensure-callback-values (session log-callback connect-callback message-callback subscribe-callback)
  (check-type session (not null))
  (when log-callback
    (hu.dwim.mosquitto.ffi:log-callback-set session log-callback))
  (when connect-callback
    (hu.dwim.mosquitto.ffi:connect-callback-set session connect-callback))
  (when message-callback
    (hu.dwim.mosquitto.ffi:message-callback-set session message-callback))
  (when subscribe-callback
    (hu.dwim.mosquitto.ffi:subscribe-callback-set session subscribe-callback)))

(defun lib-version ()
  (c-let ((major :int)
          (minor :int)
          (revision :int))
    (hu.dwim.mosquitto.ffi:lib-version (major &) (minor &) (revision &))
    (values major minor revision)))

(defun lib-init (&key minimum-version)
  (check-type minimum-version list)
  (hu.dwim.mosquitto.ffi:lib-init)
  (when minimum-version
    (destructuring-bind
          (e-major &optional e-minor e-revision)
        minimum-version
      (multiple-value-bind
            (major minor revision)
          (lib-version)
        (flet ((fail ()
                 (mosquitto-error "The Mosquitto library installed on this system is too old. Expected minimum version: ~S, actual version: ~S"
                                  minimum-version (list major minor revision))))
          (macrolet ((expect (condition)
                       `(unless ,condition
                          (fail))))
            (expect (>= major e-major))
            (expect (or (null e-minor)
                        (>= minor e-minor)))
            (expect (or (null e-revision)
                        (>= revision e-revision))))))))
  (values))

(defun %set-tls-parameters (session tls-certificate-authority tls-client-certificate tls-private-key password-callback)
  (check-type tls-certificate-authority (or null pathname))
  (check-type tls-client-certificate (or null pathname))
  (check-type tls-private-key (or null pathname))
  (multiple-value-bind
        (ca-path ca-file)
      (when tls-certificate-authority
        (if (uiop:directory-pathname-p tls-certificate-authority)
            (values (namestring tls-certificate-authority) nil)
            (values nil (namestring tls-certificate-authority))))
    (let ((result (c-fun hu.dwim.mosquitto.ffi:tls-set
                         session
                         ca-file
                         (or ca-path (cffi:null-pointer)) ;; TODO FIXME clarify with oGMo about autowrap
                         (when tls-client-certificate
                           (namestring tls-client-certificate))
                         (when tls-private-key
                           (namestring tls-private-key))
                         password-callback)))
      (unless (zerop result)
        (mosquitto-error* result "Error from Mosquitto tls-set FFI call, "))))
  (values))

(defun new-session (&key client-id (clean-session? t) user-object
                      log-callback connect-callback message-callback subscribe-callback
                      ;; TLS related args
                      tls-certificate-authority tls-client-certificate tls-private-key password-callback)
  (let ((session (c-fun/not-null hu.dwim.mosquitto.ffi:new client-id clean-session? user-object)))
    (%ensure-callback-values session log-callback connect-callback message-callback subscribe-callback)
    (%set-tls-parameters session tls-certificate-authority tls-client-certificate tls-private-key password-callback)
    session))

;; default TLS port: 8883 (http://mqtt.org/faq)
(defun connect (host &key (port 1883) (keepalive-interval 60))
  (check-type host string)
  (check-type keepalive-interval positive-integer)
  (check-type port (unsigned-byte 16))
  (let ((result (hu.dwim.mosquitto.ffi:connect (current-session) host port keepalive-interval)))
    (unless (eql result 0)
      (mosquitto-error* result "Error from Mosquitto connect FFI call. Host: ~S, port: ~S, " host port)))
  (values))

(defun process-some-events (&key (timeout 0) (count 1))
  (let ((result (hu.dwim.mosquitto.ffi:loop (current-session) timeout count)))
    (unless (zerop result)
      (mosquitto-error* result "Error from Mosquitto loop FFI call, "))
    t))

(defun publish (topic payload &key (qos 0) (retain? nil))
  (check-type topic string)
  (check-type payload (vector (unsigned-byte 8)))
  (check-type qos (integer 0 3))
  (c-let ((message-id :int))
    (cffi:with-pointer-to-vector-data (payload-pointer payload)
      (let ((result (c-fun hu.dwim.mosquitto.ffi:publish
                           (current-session)
                           (message-id &)
                           topic
                           (length payload)
                           payload-pointer
                           qos
                           (not (null retain?)))))
        (unless (eql result 0)
          (mosquitto-error* result "Error from Mosquitto publish FFI call. Payload length: ~S, " (length payload)))))
    message-id))
