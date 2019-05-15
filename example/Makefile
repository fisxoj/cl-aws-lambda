
include .env

FUNCTION_NAME ?= hello-cl
ROLE_ARN ?= "set-your-arn-here"
HANDLER ?= "example-lambda:hello"
REGION ?= "us-east-1"

.PHONY: default all clean clean-artifacts clean-docker build update deploy invoke
all: default

default: bootstrap

bootstrap: $(wildcard *.asd) $(wildcard *.lisp)
	docker build . -t cl-aws-builder
	docker volume create cl-aws-asdf-cache
	docker volume create cl-aws-quicklisp
	docker run --rm -it \
		-e ASDF_OUTPUT_TRANSLATIONS="/:/asdf-cache/" \
		-v ${PWD}:/work/ \
		-v cl-aws-quicklisp:/root/quicklisp \
		-v ${HOME}/quicklisp/local-projects:/root/quicklisp/local-projects \
		-v cl-aws-asdf-cache:/asdf-cache/ \
		cl-aws-builder

function.zip: bootstrap
	rm function.zip || exit 0
	zip function.zip bootstrap

build: bootstrap

runtime: function.zip

deploy: bootstrap function.zip
	aws lambda create-function --function-name ${FUNCTION_NAME} --zip-file fileb://function.zip --handler ${HANDLER} --runtime provided --role ${ROLE_ARN} --region ${REGION}

update: bootstrap function.zip
	aws lambda update-function-code --function-name ${FUNCTION_NAME} --zip-file fileb://function.zip
invoke:
	aws lambda invoke --function-name ${FUNCTION_NAME} --payload '{"name": "Matt"}' response.txt

invoke-local:
	docker run --rm -v "${PWD}":/var/task lambci/lambda:provided ${HANDLER} '{"name": "Wedge"}'

clean-artifacts:
	rm -f function.zip bootstrap

clean-docker:
	docker volume rm cl-aws-asdf-cache
	docker volume rm cl-aws-quicklisp

clean: clean-artifacts clean-docker
