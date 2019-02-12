
include .env

FUNCTION_NAME=hello-cl
ROLE_ARN ?= "set-your-arn-here"
HANDLER="example-lambda:hello"
REGION="us-east-1"

.PHONY: default all clean
all: default

default: bootstrap

bootstrap: example-lambda.asd $(wildcard %.lisp)
	./build.lisp

function.zip: bootstrap
	rm function.zip
	zip function.zip bootstrap

build: bootstrap

runtime: function.zip

deploy: bootstrap function.zip
	aws lambda create-function --function-name ${FUNCTION_NAME} --zip-file fileb://function.zip --handler ${HANDLER} --runtime provided --role ${ROLE_ARN} --region ${REGION}

update: bootstrap function.zip
	aws lambda update-function-code --function-name ${FUNCTION_NAME} --zip-file fileb://function.zip

clean:
	rm function.zip bootstrap
