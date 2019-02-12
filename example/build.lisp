#!/usr/bin/sbcl --script
(load "~/quicklisp/setup")

(push (uiop:getcwd) asdf:*central-registry*)

#+sb-core-compression
(defmethod asdf:perform ((o asdf/bundle:program-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression 9))

(ql:quickload :example-lambda)

(asdf:make :example-lambda)
