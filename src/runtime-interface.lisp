(defpackage cl-aws-lambda/runtime-interface
  (:use :cl :alexandria)
  (:import-from :cl-aws-lambda/environment
		#:*aws-lambda-runtime-api*)
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


(defvar *request-id* nil)


;; (defvar *next-invocation*
;;   '())


;; (defvar *error-schema*
;;   (sanity-clause.schema:load
;;    '(:error-message (:string :validator (:not-empty) :data-key "errorMessage" :required t)
;;      :error-type    (:string :validator (:not-empty) :data-key "errorType"    :required t)))
;;   "Schema for the JSON payload for an `Invocation Error <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-api.html#runtimes-api-invokeerror>`_.")


(defun make-runtime-url (&rest path-components)
  (declare (type (trivial-types:proper-list string)))

  (assert *aws-lambda-runtime-api* nil "Tried to assemble a runtime url, but *aws-lambda-runtime-api* was nil.")

  (format nil "http://~a/~a/~a"
	  *aws-lambda-runtime-api*
          *api-version*
          (str:concat path-components)))


(defun next-invocation ()
  "`Next Invocation <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-api.html#runtimes-api-next>`_."

  (multiple-value-bind (body status headers) (dex:get (make-runtime-url "runtime/invocation/next"))
    (assert (= status 200) nil "The runtime interface returned a value of ~d, the body of the response was: ~S" status body)

    (values (jojo:parse body :as :alist) (gethash "Lambda-Runtime-Aws-Request-Id" headers))))


(defmacro do-events ((event) &body body)
  (with-gensyms (request-id)
    `(loop
       for (,event ,request-id) = (multiple-value-list (next-invocation))
       do (let ((*request-id* ,request-id))
	    ,@body))))


(defun invocation-response (content)
  "`Invocation Response <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-api.html#runtimes-api-response>`_."

  (assert *request-id* nil "Tried to report an invocation error but *request-id* was unbound.")

  (dex:post (make-runtime-url "runtime/invocation/")
            :content content))


(defun invocation-error (error)
  (declare (type runtime-error error))

  (assert *request-id* nil "Tried to report an invocation error but *request-id* was unbound.")

  (dex:post (make-runtime-url "runtime/invocation/" *request-id* "error"
			      :content (jojo:with-output-to-string*
					 (jojo:with-object
					   (jojo:write-key-value "errorMessage" (or (message-of error) ""))
					   (jojo:write-key-value "errorType" (class-name (class-of error))))))))

(defun initialization-error (error)
  (declare (type runtime-error error))

  (dex:post (make-runtime-url "runtime/init/error"
			      :content (jojo:with-output-to-string*
					 (jojo:with-object
					   (jojo:write-key-value "errorMessage" (or (message-of error) ""))
					   (jojo:write-key-value "errorType" (class-name (class-of error))))))))
