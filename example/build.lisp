#!/usr/local/bin/sbcl --script
(load "~/quicklisp/setup")

(push "/work/" asdf:*central-registry*)

;; #+sb-core-compression
;; (defmethod asdf:perform ((o asdf/bundle:program-op) (c asdf:system))
;;   (uiop:dump-image (asdf:output-file o c) :executable t :compression 9))

(let ((system-name (uiop:make-symbol* (string-upcase (if (uiop:getenvp "LAMBDA_SYSTEM_NAME")
                                                         (uiop:getenv "LAMBDA_SYSTEM_NAME")
                                                         "example-lambda")))))
  (ql:quickload system-name)

  #+sbcl
  (asdf:make system-name)

  #+ccl
  (ccl:save-application "bootstrap" :toplevel-function #'cl-aws-lambda/runtime:main :prepend-kernel t)
  )
