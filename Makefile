SHELL = /bin/sh
PATH != eval "`opam env`" ; echo "$${PATH}"
export PATH

OCAMLFLAGS = -for-pack Aaa -principal   # add other options for ocamlc here

# The list of object files for AAA
AAA_SUBMODULES != echo [A-Z]*.ml
AAA_INTERFACES = $(AAA_SUBMODULES:.ml=.mli)

AAA_NATIVEOBJS != ./linkorder.sh native $(AAA_SUBMODULES) $(AAA_INTERFACES)
AAA_BYTEOBJS != ./linkorder.sh byte $(AAA_SUBMODULES) $(AAA_INTERFACES)

.PHONY: byte native

byte: Aaa.cma

native: Aaa.cmxa

Aaa.cma: Aaa.cmo
		ocamlc -a -o Aaa.cma Aaa.cmo

Aaa.cmxa: Aaa.cmx
		ocamlopt -a -o Aaa.cmxa Aaa.cmx

Aaa.cmo: $(AAA_BYTEOBJS)
		ocamlc -pack -o Aaa.cmo $(AAA_BYTEOBJS)

Aaa.cmx: $(AAA_NATIVEOBJS)
		ocamlopt -pack -o Aaa.cmx $(AAA_NATIVEOBJS)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
		ocamlc $(OCAMLFLAGS) -c -o $@ $<

.ml.cmx:
		ocamlopt $(OCAMLFLAGS) -c -o $@ $<

.mli.cmi:
		ocamlc $(OCAMLFLAGS) -c -o $@ $<

# Dependencies
.depend: Makefile $(AAA_INTERFACES) $(AAA_SUBMODULES)
		ocamldep $(AAA_INTERFACES) $(AAA_SUBMODULES) > .depend

include .depend

# Test suite
_RunTests: $(AAA_BYTEOBJS) _RunTests.ml
		ocamlfind ocamlc -package qcheck-core -package qcheck-core.runner -o _RunTests -linkpkg $(AAA_BYTEOBJS) RunTests.ml

.PHONY: clean test

test: _RunTests
		./_RunTests

clean:
		rm -f *.cma *.cmxa *.cmo *.o *.cmx *.cmi .depend _RunTests
