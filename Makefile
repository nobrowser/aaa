SHELL = /bin/sh
PATH != eval "`opam env`" ; echo "$${PATH}"
export PATH

OCAMLFLAGS = -no-alias-deps -w -49   # add other options for ocamlc here

# The list of object files for AAA
AAA_SUBMODULES != echo Aaa__*.ml
AAA_INTERFACES = $(AAA_SUBMODULES:.ml=.mli)

AAA_NATIVEOBJS != ./linkorder.sh native Aaa $(AAA_SUBMODULES) $(AAA_INTERFACES)
AAA_BYTEOBJS != ./linkorder.sh byte Aaa $(AAA_SUBMODULES) $(AAA_INTERFACES)

.PHONY: byte native

byte: Aaa.cma

native: Aaa.cmxa

Aaa.cma: $(AAA_BYTEOBJS) Aaa.cmo
		ocamlc -a -o Aaa.cma $(OCAMLFLAGS) $(AAA_BYTEOBJS) Aaa.cmo

Aaa.cmxa: $(AAA_NATIVEOBJS) Aaa.cmx
		ocamlopt -a -o Aaa.cmxa $(OCAMLFLAGS) $(AAA_NATIVEOBJS) Aaa.cmx

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
		ocamlc $(OCAMLFLAGS) -open Aaa -c $<

.ml.cmx:
		ocamlopt $(OCAMLFLAGS) -open Aaa -c $<

Aaa.cmo: Aaa.ml Aaa.cmi
		ocamlc $(OCAMLFLAGS) -c Aaa.ml

Aaa.cmx: Aaa.ml Aaa.cmi
		ocamlopt $(OCAMLFLAGS) -c Aaa.ml

.mli.cmi:
		ocamlc $(OCAMLFLAGS) -open Aaa -c $<

Aaa.cmi:
		ocamlc $(OCAMLFLAGS) -c Aaa.mli

# Dependencies
.depend: Makefile $(AAA_INTERFACES) $(AAA_SUBMODULES) Aaa.mli
		ocamldep -map Aaa.mli -open Aaa $(AAA_INTERFACES) $(AAA_SUBMODULES) > .depend

include .depend

# Test suite
RunTests: Aaa.cma Runtests.ml
		ocamlfind ocamlc -package qcheck-core -package qcheck-core.runner -o RunTests -linkpkg Aaa.cma RunTests.ml

.PHONY: clean test

test: RunTests
		./RunTests

clean:
		rm -f *.cma *.cmxa *.cmo *.o *.cmx *.cmi .depend RunTests
