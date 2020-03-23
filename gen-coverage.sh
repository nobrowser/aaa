#!/usr/local/bin/dash -Cefu

cd _build/coverage/t/
rm -f ./lib
ln -s ../lib ./lib
bisect-ppx-report html
