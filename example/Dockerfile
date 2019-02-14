FROM amazonlinux:2017.03

RUN yum install -y tar bzip2 make # gcc @development-tools
RUN mkdir /build

WORKDIR /build

COPY ./install-sbcl.sh /build/
RUN ./install-sbcl.sh

CMD ./build.lisp
