(defpackage cl-aws-lambda/environment
  (:use :cl :alexandria)
  (:import-from #:cl-aws-lambda/conditions
                #:environment-error)
  (:export #:with-environment
           #:*handler*
           #:*lambda-runtime-dir*
           #:*lambda-task-root*
           #:*aws-lambda-runtime-api*))

(in-package :cl-aws-lambda/environment)

(defvar *handler* nil
  "The handler specified by the lambda function's configuration, should be an external symbol of a package, ie. something of the form ``package:function-name``.  Case is not significant.")


(defvar *lambda-task-root* nil
  "The function that contains the function code.")


(defvar *lambda-runtme-dir* nil)


(defun check-environment ()
  "Checks to see that everything outside the lisp runtime is set up as we expect."

  (flet ((check-env (name)
           (unless (uiop:getenvp name)
             (error 'environment-error
                    :message (format nil "Environment variable ~S was not set during initialization." name)))))
    (check-env "_HANDLER")
    (check-env "LAMBDA_TASK_ROOT")))


(defmacro with-environment (() &body body)
  `(let ((*handler* (uiop:getenv "_HANDLER"))
         (*lambda-task-root* (uiop:getenv "LAMBDA_TASK_ROOT"))
         (*lambda-runtime-dir* (uiop:getenv "LAMBDA_RUNTIME_DIR")))

     (check-environment)
     ,@body))
