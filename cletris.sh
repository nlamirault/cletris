#!/bin/bash
#
# Climon tool
# Copyright (c) 2013 Nicolas Lamirault <nicolas.lamirault@gmail.com>
#

set -e
set -x
export TEST_ENV=$PWD/.lispenv

rm -fr $TEST_ENV
rm -fr ~/quicklisp
mkdir $TEST_ENV

cp misc/init.lisp $TEST_ENV
cd $TEST_ENV
wget -q http://beta.quicklisp.org/quicklisp.lisp -O quicklisp.lisp
sbcl --script init.lisp
ln -s $PWD/.. ./.quicklisp/local-projects/cletris
sbcl --script ../test/runtests.lisp
