#!/bin/bash
# Run benchmarks against the server given in $1

if [ -z "$1" ]; then
    echo "Usage: run_tests.sh <host>"
    echo "where host can be either host or host:port."
    exit 1
fi

HOST=$1
BASE=http://${HOST}

for concurrent in 1 10 100 1000 10000; do
    echo TEST ab -n 10000 -c $concurrent $BASE/1k
    ab -n 10000 -c $concurrent $BASE/1k
done

