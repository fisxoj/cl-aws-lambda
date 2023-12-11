(uiop:define-package example-lambda
  (:use :cl)
  (:import-from :cl-aws-lambda/runtime-interface
   :request-id-of
                :*context*)
  (:export #:hello))

(in-package :example-lambda)

(declaim (optimize (space 3) (speed 0)))

(defmethod request-id-of ((object t))
  "some-id")

(defun hello (event)
  (let ((name (cdr (assoc "name" event :test #'string=))))
    (if name
        `( ("statusCode" . 200)
           ("body" .
                   (("response" . ,(format nil "Hello, ~a!" name))
                    ("request-id" . ,(request-id-of *context*)))))
        (error "No name supplied!"))))
