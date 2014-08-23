#!/bin/bash
# Run benchmarks against the server given in $1

if [ -z "$1" ]; then
    echo "Usage: run_tests.sh <host>"
    echo "where host can be either host or host:port."
    exit 1
fi

HOST=$1
BASE=http://${HOST}

ab -n 10000 -c 1 $BASE/1k
ab -n 1000 -c 10 $BASE/1k
ab -n 100 -c 100 $BASE/1k
ab -n 10 -c 1000 $BASE/1k
ab -n 1 -c 10000 $BASE/1k
