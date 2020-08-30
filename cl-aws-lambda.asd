(defsystem cl-aws-lambda
  :license "MIT"
  :pathname "src/"
  :depends-on ("alexandria"
               "dexador"
               "jonathan"
	       "trivial-types"
               "vom"
               "vom-json")
  :components ((:file "conditions")
	       (:file "environment")
	       (:file "runtime-interface")
	       (:file "runtime"))
  :in-order-to ((test-op (test-op cl-aws-lambda/test))))


(defsystem cl-aws-lambda/test
  :pathname "t/"
  :depends-on ("cl-aws-lambda"
               "rove")
  :components ((:file "runtime-interface"))
  :perform (test-op (o c)
                    (funcall (read-from-string "ROVE:RUN") c :env '(("AWS_LAMBDA_RUNTIME_API" . "test-aws-runtime:8000")))))


(defsystem cl-aws-lambda/asdf
  :depends-on ("asdf")
  :pathname "src/"
  :components ((:file "asdf")))
