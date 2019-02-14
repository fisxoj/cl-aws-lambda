(defpackage cl-aws-lambda/runtime-interface
  (:use :cl :alexandria)
  (:import-from :cl-aws-lambda/conditions
		#:runtime-error
		#:message-of)
  (:export #:*request-id*
	   #:*api-version*
	   #:next-invocation
	   #:invocation-response
	   #:invocation-error
	   #:initialization-error
	   #:do-events))

(in-package :cl-aws-lambda/runtime-interface)

(defvar *api-version* "2018-06-01")


(defvar *request-id* nil
  "The value of the ``lambda-runtime-aws-request-id`` header for a given request.  Only bound inside :macro:`do-events`.")


(defvar *aws-lambda-runtime-api* nil
  "The hostname/port of the lambda runtime.")


(defun make-runtime-url (&rest path-components)
  (declare (type (trivial-types:proper-list string) path-components))
  (let ((*aws-lambda-runtime-api* (uiop:getenv "AWS_LAMBDA_RUNTIME_API")))

    (assert *aws-lambda-runtime-api* nil "Tried to assemble a runtime url, but *aws-lambda-runtime-api* was nil.")

    (format nil "http://~a/~a/~a"
	    *aws-lambda-runtime-api*
	    *api-version*
	    (apply #'str:concat path-components))))


(defun next-invocation ()
  "`Next Invocation <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-api.html#runtimes-api-next>`_."

  (multiple-value-bind (body status headers) (dex:get (make-runtime-url "runtime/invocation/next"))
    (assert (= status 200) nil "The runtime interface returned a value of ~d, the body of the response was: ~S" status body)

    (values (jojo:parse body :as :alist) (gethash "lambda-runtime-aws-request-id" headers))))


(defmacro do-events ((event) &body body)
  (with-gensyms (request-id)
    `(loop
       for (,event ,request-id) = (multiple-value-list (next-invocation))
       do (let ((*request-id* ,request-id))
	    ,@body))))


(defun invocation-response (content)
  "`Invocation Response <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-api.html#runtimes-api-response>`_."

  (assert *request-id* nil "Tried to report an invocation error but *request-id* was unbound.")

  (dex:post (make-runtime-url "runtime/invocation/" *request-id* "/response")
            :content content))


(defun invocation-error (error)
  (declare (type runtime-error error))

  (assert *request-id* nil "Tried to report an invocation error but *request-id* was unbound.")

  (dex:post (make-runtime-url "runtime/invocation/" *request-id* "/error")
	    :content (jojo:with-output-to-string*
		       (jojo:with-object
			   (jojo:write-key-value "errorMessage" (or (message-of error) ""))
			 (jojo:write-key-value "errorType" (class-name (class-of error)))))))

(defun initialization-error (error)
  (declare (type runtime-error error))

  (dex:post (make-runtime-url "runtime/init/error")
	    :content (jojo:with-output-to-string*
		       (jojo:with-object
			   (jojo:write-key-value "errorMessage" (or (message-of error) ""))
			 (jojo:write-key-value "errorType" (class-name (class-of error)))))))
