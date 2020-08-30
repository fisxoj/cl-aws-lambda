(defpackage cl-aws-lambda/runtime-interface
  (:use :cl :alexandria)
  (:import-from :cl-aws-lambda/conditions
		#:runtime-error
		#:message-of)
  (:local-nicknames (:log :vom))
  (:export #:*api-version*
           #:*context*

           ;; Context readers
           #:request-id-of
           #:deadline-milliseconds-of
           #:invoked-function-arn-of
           #:trace-id-of
	   #:client-context-of
	   #:cognito-identity-of

           #:next-invocation
           #:invocation-response
	   #:invocation-error
	   #:initialization-error
	   #:do-events))

(in-package :cl-aws-lambda/runtime-interface)


(defvar *api-version* "2018-06-01")


(defvar *aws-lambda-runtime-api* nil
  "The hostname/port of the lambda runtime.")


(defvar *context* nil
  "Instance of :class:`request-context` that is bound during the life of a request.  Contains some meta-information about the request.")


(defvar +lisp-lambda-runtime-agent+ (load-time-value (format nil "cl-aws-lambda (~a ~a)" (uiop/common-lisp:lisp-implementation-type) (lisp-implementation-version))))


(defclass request-context ()
  ((request-id :type string
               :initarg :request-id
               :reader request-id-of
               :documentation "Lambda-Runtime-Aws-Request-Id – The request ID, which identifies the request that triggered the function invocation.

For example, 8476a536-e9f4-11e8-9739-2dfe598c3fcd.")

   (deadline-milliseconds :type (integer 0)
                          :initarg :deadline-ms
                          :reader deadline-milliseconds-of
                          :documentation "Lambda-Runtime-Deadline-Ms – The date that the function times out in Unix time milliseconds.

For example, 1542409706888.")

   (invoked-function-arn :type string
                         :initarg :invoked-function-arn
                         :reader invoked-function-arn-of
                         :documentation "Lambda-Runtime-Invoked-Function-Arn – The ARN of the Lambda function, version, or alias that's specified in the invocation.

For example, arn:aws:lambda:us-east-2:123456789012:function:custom-runtime.")

   (trace-id :type string
             :initarg :trace-id
             :reader trace-id-of
             :documentation "Lambda-Runtime-Trace-Id – The AWS X-Ray tracing header.

For example, Root=1-5bef4de7-ad49b0e87f6ef6c87fc2e700;Parent=9a9197af755a6419;Sampled=1.")

   (client-context :type t
                   :initarg :client-context
                   :reader client-context-of
                   :documentation "Lambda-Runtime-Client-Context – For invocations from the AWS Mobile SDK, data about the client application and device.")

   (cognito-identity :type t
                     :initarg :cognito-identity
                     :reader cognito-identity-of
                     :documentation "Lambda-Runtime-Cognito-Identity – For invocations from the AWS Mobile SDK, data about the Amazon Cognito identity provider.")))


(declaim (inline make-context))
(defun make-context (headers)
  "Makes a context instance out of a hash table of headers."

  (declare (optimize (speed 3) (space 3) (safety 0) (compilation-speed 0)))

  (macrolet ((header (name) `(gethash ,(string-downcase name) headers)))

    (make-instance 'request-context
                   :request-id (header "Lambda-Runtime-Aws-Request-Id")
                   :deadline-ms (header "Lambda-Runtime-Deadline-Ms")
                   :invoked-function-arn (header "Lambda-Runtime-Invoked-Function-Arn")
                   :trace-id (header "Lambda-Runtime-Trace-Id")
                   :client-context (header "Lambda-Runtime-Client-Context")
                   :cognito-identity (header "Lambda-Runtime-Cognito-Identity"))))


(declaim (inline sampled-p))
(defun sampled-p (context)
  (declare (type request-context context))

  (if (search "Sampled=1" (trace-id-of context))
      t
      nil))


(declaim (inline make-runtime-url))
(defun make-runtime-url (&rest path-components)
  (declare (type (trivial-types:proper-list string) path-components))
  (let ((*aws-lambda-runtime-api* (uiop:getenv "AWS_LAMBDA_RUNTIME_API")))

    (assert *aws-lambda-runtime-api* nil "Tried to assemble a runtime url, but *aws-lambda-runtime-api* was nil.")

    (format nil "http://~a/~a/~{~a~}"
	    *aws-lambda-runtime-api*
	    *api-version*
	    path-components)))


;;; BIG HONKING WARNING FOR ALL RUNTIME API CALLS:
;;; Turn off all timeouts, it fixes problems with timing out when the application is frozen/thawed by the lambda runtime.
;;; source: https://github.com/bmoffatt/aws-lambda-rust-runtime/blob/abe774a359b33f82319b404209a3d5ae30710d55/lambda-runtime-client/src/client.rs#L134-L139


(declaim (inline next-invocation))
(defun next-invocation ()
  "`Next Invocation <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-api.html#runtimes-api-next>`_."

  (declare (optimize (speed 3) (space 3) (safety 1) (compilation-speed 0)))

  (multiple-value-bind (body status headers) (dex:get (make-runtime-url "runtime/invocation/next")
                                                      :headers `((:user-agent . +lisp-lambda-runtime-agent+))
                                                      :keep-alive t
                                                      :connect-timeout nil
                                                      :read-timeout nil)
    (declare (type fixnum status))
    (assert (= status 200) nil "The runtime interface returned a value of ~d, the body of the response was: ~S" status body)

    (log:debug "next-invocation result: ~S ~S ~S" body status headers)

    (values (jojo:parse body :as :alist) (make-context headers))))


(defmacro do-events ((event) &body body)
  (with-gensyms (context)
    `(loop
       for (,event ,context) = (multiple-value-list (next-invocation))
       do (let ((*context* ,context))

            ;; When the request is traced, we should set _X_AMZN_TRACE_ID so that xray libraries know the tracing info.
            (when (sampled-p *context*)
              (setf (uiop:getenv "_X_AMZN_TRACE_ID") (trace-id-of *context*)))

	    ,@body))))


(declaim (inline invocation-response))
(defun invocation-response (content)
  "`Invocation Response <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-api.html#runtimes-api-response>`_."

  (declare (optimize (speed 3) (space 3) (safety 1) (compilation-speed 0)))

  (assert *context* nil "Tried to report an invocation error but *context* was unbound.")

  (log:debug "Responding to event with ~S." content)

  (multiple-value-bind (body status headers)
      (dex:post (make-runtime-url "runtime/invocation/" (request-id-of *context*) "/response")
                :headers `((:content-type . "application/json")
                           (:user-agent . ,+lisp-lambda-runtime-agent+))
                :content (jojo:to-json content :from :alist)
                ;; Seems to save around 10ms
                :force-binary t
                :connect-timeout nil
                :read-timeout nil)

    (log:debug "invocation-response result: ~S ~S ~S" body status headers)))


(defun invocation-error (error)
  ;; (declare (type runtime-error error))

  (assert *context* nil "Tried to report an invocation error but *context* was unbound.")

  (log:error "Invocation Error [~a]: ~a" (class-name (class-of error)) error)

  (multiple-value-bind (body status headers)
      (dex:post (make-runtime-url "runtime/invocation/" (request-id-of *context*) "/error")
                :headers `((:user-agent . ,+lisp-lambda-runtime-agent+)
                           (:lambda-runtime-function-error-type . "Unhandled"))
                :content (jojo:with-output-to-string*
                           (jojo:with-object
                             (jojo:write-key-value "errorMessage" (or (message-of error) ""))
                             (jojo:write-key-value "errorType" (class-name (class-of error)))))
                :keep-alive t
                :connect-timeout nil
                :read-timeout nil)

    (log:debug "invocation-error result: ~S ~S ~S" body status headers)))


(defun initialization-error (error)
  ;; (declare (type runtime-error error))

  (log:error "Initialization Error [~a]: ~a" (class-name (class-of error)) error)

  (multiple-value-bind (body status headers)
      (dex:post (make-runtime-url "runtime/init/error")
                :headers `((:user-agent . ,+lisp-lambda-runtime-agent+)
                           (:lambda-runtime-function-error-type . "Unhandled"))
                :content (jojo:with-output-to-string*
                           (jojo:with-object
                             (jojo:write-key-value "errorMessage" (or (message-of error) ""))
                             (jojo:write-key-value "errorType" (class-name (class-of error)))))
                :keep-alive t
                :connect-timeout nil
                :read-timeout nil)

    (log:debug "initialization-error result: ~S ~S ~S" body status headers))

  (uiop:quit 1))
