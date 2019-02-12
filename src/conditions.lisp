(defpackage cl-aws-lambda/conditions
  (:use :cl :alexandria)
  (:export #:runtime-error
	   #:message-of
	   #:environment-error
	   #:uncaught-error))

(in-package :cl-aws-lambda/conditions)

(define-condition runtime-error (error)
  ((message :type (or null string)
	    :reader message-of
	    :initarg :message
	    :documentation "A user-readable message to be returned as the result of a lambda invocation."))
  (:documentation "The base class for all errors that can be the result of a lambda invocation."))


(define-condition environment-error (runtime-error)
  ()
  (:documentation "An error that indicates something is wrong with the environment variables that set up the runtime."))


(define-condition uncaught-error (runtime-error)
  ()
  (:documentation "An error that the user didn't handle explicitly was thrown and the runtime caught it."))
