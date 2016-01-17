;;; This file is loaded before the generated FFI.

(in-package :hu.dwim.mosquitto)

(defun ffi-name-transformer (name kind &key &allow-other-keys)
  (check-type name string)
  (let ((prefix "mosquitto_"))
    (cond
      ((starts-with-subseq prefix name)
       (subseq name (length prefix)))
      (t
       name))))

#+nil ;; we're fine with the default for now
(defun ffi-type-transformer (type context &rest args &key &allow-other-keys)
  (let ((type (apply 'cffi/c2ffi:default-ffi-type-transformer type context args)))
    (cond
      ((and (eq type :int)
            (consp context)
            (eq (first context) :function)
            (eq (third context) :return-type)
            (starts-with-subseq "hci_" (second context))
            (not (member (second context)
                         ;; a blacklist of hci function names (as a string)
                         ;; that should automatically signal error on failure.
                         '()
                         :test 'equal)))
       ;; this is a cffi type that automatically signals
       ;; an error if the return code is negative.
       'hci-return-code)
      #+nil
      ((equal context '(:struct "hci_dev_info" "name"))
       (assert (equal type '(:array :char 8)))
       ;; err, no, this dereferences a pointer
       :string)
      (t
       type))))
