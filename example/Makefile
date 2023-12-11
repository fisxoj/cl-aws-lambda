include .env

FUNCTION_NAME ?= hello-cl
ROLE_ARN ?= "set-your-arn-here"
HANDLER ?= "example-lambda:hello"
REGION ?= "us-east-1"

RUNC ?= docker

PAYLOAD ?= '{"name": "Matt"}'

.DEFAULT_GOAL := help

.PHONY: default all clean clean-artifacts clean-container update deploy help invoke

all: default

help: ## Display this help section
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z0-9_-]+:.*?## / {printf "\033[36m%-38s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

bootstrap: $(wildcard *.asd) $(wildcard *.lisp) ## Build a lisp image as a provided runtime
	$(RUNC) build ../ -t cl-lambda-builder
	$(RUNC) volume create cl-aws-asdf-cache || true
	$(RUNC) volume create cl-aws-quicklisp || true
	$(RUNC) run --platform=linux/amd64 --rm -it \
		-e ASDF_OUTPUT_TRANSLATIONS="/:/asdf-cache/" \
		-v ${PWD}:/work/:Z \
		-v ${PWD}/.qlot:/root/quicklisp \
		-v cl-aws-asdf-cache:/asdf-cache/ \
		cl-lambda-builder

function.zip: bootstrap
	rm function.zip || exit 0
	zip function.zip bootstrap

deploy: bootstrap function.zip ## Create a function.zip payload and create a new lambda function in AWS
	aws --profile ${AWS_PROFILE} lambda create-function \
		--function-name ${FUNCTION_NAME} \
		--zip-file fileb://function.zip \
		--handler ${HANDLER} \
		--runtime provided.al2023 \
		--role ${ROLE_ARN} \
		--region ${REGION}

update: bootstrap function.zip ## Create a function.zip and update the existing lambda function in AWS
	aws --profile ${AWS_PROFILE} lambda update-function-code \
		--function-name ${FUNCTION_NAME} \
		--zip-file fileb://function.zip \
		--region ${REGION}

invoke: ## Invoke the function with a given payload
	aws --profile ${AWS_PROFILE} lambda invoke \
		--function-name ${FUNCTION_NAME} \
		--cli-binary-format raw-in-base64-out \
		--payload ${PAYLOAD} \
		--region ${REGION} \
		response.txt

invoke-local: bootstrap ## Invoke the function locally using a container
	$(RUNC) run --platform=linux/amd64 --rm -v "${PWD}":/var/task:Z public.ecr.aws/lambda/provided:al2023 ${HANDLER} ${PAYLOAD}

clean-artifacts: ## Remove compiled artifacts
	rm -f function.zip bootstrap

clean-container: ## Remove container cache volumes
	$(RUNC) volume rm cl-aws-asdf-cache
	$(RUNC) volume rm cl-aws-quicklisp

clean: clean-artifacts clean-container ## Remove everything
