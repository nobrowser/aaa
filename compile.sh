#! /bin/sh -e

BYTE_OR_NATIVE="$1"
COMMAND="$2"
TARGET="$3"
shift 3

case "${BYTE_OR_NATIVE}" in
    ([Bb]*)
        compiler=ocamlc
        COMPILER=OCAMLC
        ;;
    ([Nn]*)
        compiler=ocamlopt
        COMPILER=OCAMLOPT
        ;;
    (*)
        printf 'Invalid byte-or-native argument: %s\n' "${BYTE-OR-NATIVE}"
        exit 2
esac

announce()
{
    case "${VERBOSE_ANNOUNCE:-}" in
        ('')
            printf '%s %s -o %s\n' "$COMPILER" "$COMMAND" "$TARGET" ;;
        (*)
            printf '%s %s -o %s' "$COMPILER" "$COMMAND" "$TARGET"
            printf ' %s' "$@"
            printf '\n'
            ;;
    esac
}

announce "$@"
ocamlfind "$compiler" $(printf ' -package %s' $(cat packages)) "$COMMAND" -o "$TARGET" "$@"
