(defpackage example-lambda
  (:use :cl)
  (:export #:hello))

(in-package :example-lambda)

(defun hello (event)
  (let ((name (cdr (assoc "name" event :test #'string=))))
    (if name
	(with-output-to-string (s)
          (jonathan.encode:with-output (s)
            (jonathan:with-object
              (jojo:write-key-value "response" (format nil "Hello, ~a!" name))
              (jojo:write-key-value "request-id" (cl-aws-lambda/runtime-interface:request-id-of cl-aws-lambda/runtime-interface:*context*)))))
	(error "No name supplied!"))))
