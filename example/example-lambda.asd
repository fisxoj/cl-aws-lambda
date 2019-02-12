(defsystem example-lambda
  :depends-on ("cl-aws-lambda")
  :components ((:file "hello"))
  :build-operation "program-op"
  :build-pathname "bootstrap"
  :entry-point "cl-aws-lambda/runtime:main")
