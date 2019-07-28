#! /bin/sh -e

LIBDIR="$1" ; shift
cp Aaa.cma Aaa.cmxa Aaa__*.mli Aaa__*.cmi META "${LIBDIR}"
