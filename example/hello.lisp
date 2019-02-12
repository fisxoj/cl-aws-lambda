(defpackage example-lambda
  (:use :cl)
  (:export #:hello))

(in-package :example-lambda)

(defun hello (event)
  (let ((name (cdr (assoc "name" event :test #'string=))))
    (if name
	(format nil "Hello, ~a!" name)
	(error "No name supplied!"))))
