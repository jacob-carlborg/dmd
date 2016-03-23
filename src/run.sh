#!/bin/sh

set -e

./build.sh
./dmd -conf=dmd.conf-b -g main.d
./main
