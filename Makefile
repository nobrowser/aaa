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
		@ ./compile.sh byte -a Aaa.cma Aaa.cmo

Aaa.cmxa: Aaa.cmx
		@ ./compile.sh native -a Aaa.cmxa Aaa.cmx

Aaa.cmo: $(AAA_BYTEOBJS)
		@ ./compile.sh byte -pack Aaa.cmo $(AAA_BYTEOBJS)

Aaa.cmx: $(AAA_NATIVEOBJS)
		@ ./compile.sh native -pack Aaa.cmx $(AAA_NATIVEOBJS)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
		@ ./compile.sh byte -c $@ $(OCAMLFLAGS) $<

.ml.cmx:
		@ ./compile.sh native -c $@ $(OCAMLFLAGS) $<

.mli.cmi:
		@ ./compile.sh byte -c $@ $(OCAMLFLAGS) $<

# Dependencies
.depend: Makefile $(AAA_INTERFACES) $(AAA_SUBMODULES)
		ocamldep $(AAA_INTERFACES) $(AAA_SUBMODULES) > .depend

include .depend

# Test suite
_RunTests: $(AAA_BYTEOBJS) _RunTests.ml
		@ ./compile.sh byte -linkpkg _RunTests -w -24 $(AAA_BYTEOBJS) _RunTests.ml

.PHONY: clean test

test: _RunTests
		./_RunTests

clean:
		rm -f *.cma *.cmxa *.cmo *.o *.a *.cmx *.cmi .depend _RunTests
