FROM amazonlinux:2017.03.1.20170812

RUN yum install -y zip bzip2 zlib

RUN mkdir /work
RUN mkdir /scripts
WORKDIR /work

# get SBCL and ql bootstrap
RUN curl -s -f -O -L http://prdownloads.sourceforge.net/sbcl/sbcl-1.4.14-x86-64-linux-binary.tar.bz2
RUN curl -s -f -O "https://beta.quicklisp.org/quicklisp.lisp"

# install binary to /usr/local/bin/sbcl
RUN bzip2 -cd sbcl-1.4.14-x86-64-linux-binary.tar.bz2 | tar xvf - \
    && cd sbcl-1.4.14-x86-64-linux \
    && sh install.sh

# install quicklisp
RUN /usr/local/bin/sbcl --non-interactive \
        --load "quicklisp.lisp" \
        --eval "(quicklisp-quickstart:install)" \
        --eval "(ql-util:without-prompting (ql:add-to-init-file))"

COPY build.lisp /scripts/build.lisp

RUN mkdir -p /root/quicklisp/local-projects/cl-aws-lambda/

COPY ./ /root/quicklisp/local-projects/cl-aws-lambda/

RUN chmod +x /scripts/build.lisp

CMD /scripts/build.lisp && zip function.zip bootstrap