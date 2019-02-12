(defpackage cl-aws-lambda-test/runtime-interface
  (:use :cl :alexandria :rove
        :cl-aws-lambda/runtime-interface))

(in-package :cl-aws-lambda-test/runtime-interface)

(deftest test-make-runtime-url
  (ok (string-equal (cl-aws-lambda/runtime-interface::make-runtime-url "my/potato")
                    "http://test-aws-runtime:8000/2018-06-01/my/potato")
      "generates a correct url string."))
