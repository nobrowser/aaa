#! /bin/sh

tf=`mktemp /tmp/duneXXXXXXXXXXXX`
./RunTests.bc >>$tf 2>&1
TEST_STATUS=$?

case $TEST_STATUS in
    (0) rm $tf ;;
    (*) printf '%s\n' $tf;;
esac
