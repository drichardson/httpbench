#!/bin/bash
# Generate data so you don't have to check in big files to git.

set -e
mkdir -p data
cd data
dd if=/dev/urandom of=1k bs=1024 count=1
dd if=/dev/urandom of=10k bs=1024 count=10
dd if=/dev/urandom of=1gig bs=4096 count=262144
echo OK

