(defpackage cl-aws-lambda/runtime
  (:use :cl :alexandria :cl-aws-lambda/runtime-interface)
  (:import-from :cl-aws-lambda/environment
		#:with-environment
		#:*handler*
		#:*lambda-task-root*)
  (:import-from :cl-aws-lambda/conditions
		#:runtime-error
		#:uncaught-error
		#:environment-error)
  (:export #:main))

(in-package :cl-aws-lambda/runtime)


(defun main ()
  "Main entry point that bootstraps the runtime and then invokes the handler function."

  (handler-bind ((environment-error (lambda (e)
				      (initialization-error e)))
		 (error (lambda (e)
			  (initialization-error (make-instance 'uncaught-error
							       :message (format nil "Uncaught error signaled during initialization:~%  ~a~%~%" e))))))
    (with-environment ()
      (let ((handler-function (symbol-function (read-from-string *handler*))))
        (format nil "~&Using handler function ~a." *handler*)
        (do-events (event)
          (format nil "~&Handling event:~% ~a" event)
          (handler-bind ((runtime-error (lambda (e)
                                          (invocation-error e)))
                         (error (lambda (e)
                                  (invocation-error (make-instance 'uncaught-error
                                                                   :message (format nil "Uncaught error signaled during invocation:~%---~%  ~a~%---~%" e))))))
            (invocation-response (funcall handler-function event))))))))
