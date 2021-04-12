(defpackage cl-aws-lambda/runtime
  (:use :cl
        :alexandria
        :cl-aws-lambda/runtime-interface
        :cl-aws-lambda/conditions
        :cl-aws-lambda/environment)
  (:local-nicknames (:log :beaver))
  (:export #:main))

(in-package :cl-aws-lambda/runtime)


(defmacro handling-initialization-errors (() &body body)
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
  (setf beaver:*loggers*
        (list (make-instance 'beaver/st-json:st-json-logger)))
  (handling-initialization-errors ()
    (with-environment ()
      (let ((handler-function (symbol-function (read-from-string *handler*))))

        (log:info "Using handler function ~a." *handler*)

        (do-events (event)
          (log:with-fields (:request-id (request-id-of *context*) string)
            (handling-invocation-errors ()
              (invocation-response (funcall handler-function event)))))))))
