(defpackage example-lambda
  (:use :cl)
  (:export #:hello))

(in-package :example-lambda)

(defun hello (event)
  (let ((name (cdr (assoc "name" event :test #'string=))))
    (if name
        (list (cons "response" (format nil "Hello, ~a!" name))
              (cons "resquest-id" (cl-aws-lambda/runtime-interface:request-id-of cl-aws-lambda/runtime-interface:*context*)))
	(error "No name supplied!"))))
