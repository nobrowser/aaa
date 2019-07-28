#! /bin/sh -e

BYTE_OR_NATIVE="$1" ; shift
LIBNAME="$1" ; shift

case "${BYTE_OR_NATIVE}" in
    ([Bb]*)
        ocamldep -sort -map "${LIBNAME}.mli" -open "${LIBNAME}" "$@" |
            tr ' ' '\n' |
            grep 'ml$' |
            sed -e 's,ml$,cmo,'
        ;;
    ([Nn]*)
        ocamldep -sort -map "${LIBNAME}.mli" -open "${LIBNAME}" "$@" |
            tr ' ' '\n' |
            grep 'ml$' |
            sed -e 's,ml$,cmx,'
        ;;
    (*)
        printf 'Invalid byte-or-native argument: %s\n' "${BYTE-OR-NATIVE}"
        exit 2
esac
