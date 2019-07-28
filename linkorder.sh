#! /bin/sh -e

LIBNAME="$1" ; shift
ocamldep -sort -map "${LIBNAME}.mli" -open "${LIBNAME}" "$@" |
    tr ' ' '\n' |
    grep 'ml$' |
    sed -e 's,ml$,cmo,'
