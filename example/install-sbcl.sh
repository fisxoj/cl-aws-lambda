#!/bin/bash

SBCL_VERSION=1.4.14

curl http://prdownloads.sourceforge.net/sbcl/sbcl-$SBCL_VERSION-x86-64-linux-binary.tar.bz2 -L -o sbcl.tar.bz2

tar xfj sbcl.tar.bz2

cd sbcl-$SBCL_VERSION-x86-64-linux

sh install.sh

cd ..

rm -rf ./sbcl-$SBCL_VERSION-x86-64-linux
