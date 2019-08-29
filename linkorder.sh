#! /bin/sh -e

BYTE_OR_NATIVE="$1" ; shift

case "${BYTE_OR_NATIVE}" in
    ([Bb]*)
        ocamldep -sort "$@" |
            tr ' ' '\n' |
            grep 'ml$' |
            sed -e 's,ml$,cmo,'
        ;;
    ([Nn]*)
        ocamldep -sort "$@" |
            tr ' ' '\n' |
            grep 'ml$' |
            sed -e 's,ml$,cmx,'
        ;;
    (*)
        printf 'Invalid byte-or-native argument: %s\n' "${BYTE-OR-NATIVE}"
        exit 2
esac
