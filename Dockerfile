FROM --platform=linux/amd64 amazonlinux:2023

RUN yum install -y zip bzip2 zlib tar make which && yum clean all

RUN mkdir /work
RUN mkdir /scripts
WORKDIR /work

ARG SBCL_VERSION=2.3.11

# install sbcl to /usr/local/bin/sbcl
RUN curl -s -f -O -L http://prdownloads.sourceforge.net/sbcl/sbcl-$SBCL_VERSION-x86-64-linux-binary.tar.bz2
RUN bzip2 -cd sbcl-$SBCL_VERSION-x86-64-linux-binary.tar.bz2 | tar xvf - \
    && cd sbcl-$SBCL_VERSION-x86-64-linux \
    && sh install.sh \
    && rm -rf sbcl-$SBCL_VERSION-x86-64-linux sbcl-$SBCL_VERSION-x86-64-linux-binary.tar.bz2

# install quicklisp
RUN curl -s -f -O "https://beta.quicklisp.org/quicklisp.lisp" \
    && /usr/local/bin/sbcl --non-interactive \
        --load "quicklisp.lisp" \
        --eval "(quicklisp-quickstart:install)" \
        --eval "(ql-util:without-prompting (ql:add-to-init-file))" \
    && rm quicklisp.lisp

COPY build.lisp /scripts/build.lisp

RUN chmod +x /scripts/build.lisp

CMD /scripts/build.lisp && zip function.zip bootstrap
