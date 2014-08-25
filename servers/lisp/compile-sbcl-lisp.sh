#!/bin/bash

usage() {
    echo "Usage: compile-sbcl-lisp.sh <lisp-file> <out-file>"
    echo "where lisp-file contains a function called main"
    exit 1
}

[ ! -z "$1" ] || usage
[ ! -z "$2" ] || usage

sbcl --non-interactive \
    --eval "(load \"$1\")" \
    --eval "(save-lisp-and-die \"$2\" :executable t :toplevel #'main)"
