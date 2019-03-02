(defpackage cl-aws-lambda/asdf
  (:use :cl)
  (:export :lambda-system))

(in-package :cl-aws-lambda/asdf)


(defclass lambda-system (asdf:system)
  ()
  (:default-initargs
   :build-operation "program-op"
   :build-pathname "bootstrap"
   :entry-point "cl-aws-lambda/runtime:main")
  (:documentation "This class sets some defaults for the build process of the lambda to simplify setup for users."))


(defmethod asdf:component-children :around ((c lambda-system))
  (declare (ignore c))

  (list* (asdf:find-system "cl-aws-lambda") (call-next-method)))


(defmethod asdf/system:component-build-pathname ((c lambda-system))
  (asdf:system-relative-pathname c "bootstrap"))


(defmethod asdf/system:component-entry-point ((c lambda-system))
  (declare (ignore c))

  "cl-aws-lambda/runtime:main")


(defmethod asdf/component:component-build-operation ((c lambda-system))
  (declare (ignore c))

  "program-op")


(import 'lambda-system :asdf)
