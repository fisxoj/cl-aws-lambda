(defpackage cl-aws-lambda/runtime
  (:use :cl
        :alexandria
        :cl-aws-lambda/runtime-interface
        :cl-aws-lambda/conditions
        :cl-aws-lambda/environment)
  (:export #:main))

(in-package :cl-aws-lambda/runtime)


(defmacro handling-intialization-errors (() &body body)
  `(restart-case
       (handler-bind ((environment-error (lambda (e)
                                           (initialization-error e)
                                           (invoke-restart 'die)))
                      (error (lambda (e)
                               (initialization-error (handle-uncaught-initialization-error e))
                               (invoke-restart 'die))))

         ,@body)

     (die () (uiop:quit 1))))


(defmacro handling-invocation-errors (() &body body)
  `(restart-case
       (handler-bind ((runtime-error (lambda (e)
                                       (invocation-error e)
                                       (invoke-restart 'continue)))
                      (error (lambda (e)
                               (invocation-error (handle-uncaught-invocation-error e))
                               (invoke-restart 'continue))))

         ,@body)
     (continue ()
       ;; Let the iteration continue, the error will be handled in the handler-bind form
       nil)))


(defun main ()
  "Main entry point that bootstraps the runtime and then invokes the handler function."

  (declare (optimize space (speed 3)))

  (handling-intialization-errors ()
    (with-environment ()
      (let ((handler-function (symbol-function (read-from-string *handler*))))

        (log:debug "Using handler function ~a." *handler*)

        (do-events (event)
          (log:debug "~&Handling event:~% ~a" event)

          (handling-invocation-errors ()
            (invocation-response (funcall handler-function event))))))))
