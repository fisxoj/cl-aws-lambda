#!/usr/bin/sbcl --script
(load "~/quicklisp/setup")

(push (uiop:getcwd) asdf:*central-registry*)

#+sb-core-compression
(defmethod asdf:perform ((o asdf/bundle:program-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression 9))

(ql:quickload :example-lambda)

#+sbcl
(asdf:make :example-lambda)

#+ccl
(ccl:save-application "bootstrap" :toplevel-function #'cl-aws-lambda/runtime:main :prepend-kernel t)
