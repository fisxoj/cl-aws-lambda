(defpackage cl-aws-lambda/conditions
  (:use :cl :alexandria)
  (:local-nicknames (:log :beaver))
  (:export #:runtime-error
           #:message-of
           #:environment-error))

(in-package :cl-aws-lambda/conditions)

(define-condition runtime-error (error)
  ((message :type (or null string)
            :reader message-of
            :initarg :message
            :documentation "A user-readable message to be returned as the result of a lambda invocation."))
  (:report (lambda (c s)
             (format s "Condition ~S was signalled~@[ ~a~]"
                     (class-name (class-of c))
                     (when (slot-boundp c 'message)
                       (slot-value c 'message)))))
  (:documentation "The base class for all errors that can be the result of a lambda invocation."))


(define-condition environment-error (runtime-error)
  ()
  (:documentation "An error that indicates something is wrong with the environment variables that set up the runtime."))
