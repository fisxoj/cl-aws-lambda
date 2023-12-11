(defpackage example-lambda
  (:use :cl)
  (:export #:hello))

(in-package :example-lambda)

(declaim (optimize (space 3) (speed 0)))

(defun hello (event)
  (let ((name (cdr (assoc "name" event :test #'string=))))
    (if name
        (list (cons "response" (format nil "Hello, ~a!" name))
              (cons "request-id" (cl-aws-lambda/runtime-interface:request-id-of cl-aws-lambda/runtime-interface:*context*)))
        (error "No name supplied!"))))
