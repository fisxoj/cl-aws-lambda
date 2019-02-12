(defpackage cl-aws-lambda/environment
  (:use :cl :alexandria)
  (:import-from #:cl-aws-lambda/conditions
		#:environment-error)
  (:export #:with-environment
	   #:*handler*
	   #:*lambda-task-root*
	   #:*aws-lambda-runtime-api*))

(in-package :cl-aws-lambda/environment)

(defvar *handler* nil
  "The handler specified by the lambda function's configuration, should be an external symbol of a package, ie. something of the form ``package:function-name``.  Case is not significant.")


(defvar *lambda-task-root* nil
  "The function that contains the function code.")


(defvar *aws-lambda-runtime-api*)


(defun check-environment ()
  "Checks to see that everything outside the lisp runtime is set up as we expect."

  (flet ((check-env (name)
	   (unless (uiop:getenvp name)
	     (error 'environment-error
		    :message (format nil "Environment variable ~S was not set during initialization." name)))))
    (check-env "_HANDLER")
    (check-env "LAMBDA_TASK_ROOT")
    (check-env "AWS_LAMBDA_RUNTIME_API")))


(defmacro with-environment (() &body body)
  `(let ((*handler* (uiop:getenv "_HANDLER"))
	 (*lambda-task-root* (uiop:getenv "LAMBDA_TASK_ROOT"))
	 (*aws-lambda-runtime-api* (uiop:getenv "AWS_LAMBDA_RUNTIME_API")))
     ;; (declare (dynamic-extent *handler* *lambda-task-root* *aws-lambda-runtime-api*))
     (check-environment)
     ,@body))
