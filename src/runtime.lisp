(defpackage cl-aws-lambda/runtime
  (:use :cl
        :alexandria
        :cl-aws-lambda/runtime-interface
        :cl-aws-lambda/conditions
        :cl-aws-lambda/environment)
  (:export #:main))

(in-package :cl-aws-lambda/runtime)


(defun main ()
  "Main entry point that bootstraps the runtime and then invokes the handler function."

  (handler-bind ((environment-error (lambda (e)
                                      (initialization-error e)
                                      (uiop:quit 1)))
		 (error (lambda (e)
                          (handle-uncaught-initialization-error e)
                          (uiop:quit 1))))

    (with-environment ()
      (let ((handler-function (symbol-function (read-from-string *handler*))))

        (log:info "Using handler function ~a." *handler*)

        (do-events (event)
          (log:debug "~&Handling event:~% ~a" event)

          (restart-case
              (handler-bind ((runtime-error (lambda (e)
                                              (invocation-error e)
                                              (invoke-restart 'continue)))
                             (error (lambda (e)
                                      (invocation-error (handle-uncaught-invocation-error e))
                                      (invoke-restart 'continue))))

                (invocation-response (funcall handler-function event)))
            (continue ()
              ;; Let the iteration continue, the error will be handled in the handler-bind form
              nil)))))))
