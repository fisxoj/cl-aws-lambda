(defpackage cl-aws-lambda/runtime
  (:use :cl :alexandria :cl-aws-lambda/runtime-interface)
  (:import-from :cl-aws-lambda/environment
		#:with-environment
		#:*handler*
		#:*lambda-task-root*
		#:*aws-lambda-runtime-api*)
  (:import-from :cl-aws-lambda/conditions
		#:runtime-error
		#:uncaught-error
		#:environment-error)
  (:export #:main))

(in-package :cl-aws-lambda/runtime)


(defun main ()
  "Main entry point that bootstraps the runtime and then invokes the handler function."

  (handler-case
      (with-environment ()
	(let ((handler-function (symbol-function (read-from-string *handler*))))
	  (format nil "~&Using handler function ~a." *handler*)
	  (do-events (event)
	    (format nil "~&Handling event:~% ~a" event)
	    (handler-case (invocation-response (funcall handler-function event))
	      (runtime-error (e)
		(invocation-error e))
	      (t (e)
		(invocation-error (make-instance 'uncaught-error
						 :message (format nil "Uncaught error signaled during invocation:~%---~%  ~a~%---~%It would be better to handle this explicitly and put a useful message in a subclass of cl-aws-lambda/conditions:runtime-error." e))))))))
    (environment-error (e)
       (initialization-error e))
     (t (e)
       (initialization-error (make-instance 'uncaught-error
					    :message (format nil "Uncaught error signaled during initialization:~%  ~a~%~%It would be better to handle this explicitly and put a useful message in a subclass of cl-aws-lambda/conditions:runtime-error." e))))))
