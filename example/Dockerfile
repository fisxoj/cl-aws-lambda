FROM --platform=linux/amd64 public.ecr.aws/lambda/provided:al2023

# Copy custom runtime bootstrap
COPY bootstrap ${LAMBDA_RUNTIME_DIR}

# Set the CMD to your handler (could also be done as a parameter override outside of the Dockerfile)
CMD [ "example-lambda:hello" ]
