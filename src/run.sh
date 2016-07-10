#!/bin/sh

set -e

./build.sh
# lldb -o run -o bt -o quit -- ./dmd -conf=dmd.conf-b -g main.d
./dmd -conf=dmd.conf-b -g main.d
./main
