(defsystem example-lambda
  :defsystem-depends-on ("cl-aws-lambda/asdf")
  :class :lambda-system
  :components ((:file "hello")))
