SHELL = /bin/sh

OCAMLFLAGS = -no-alias-deps -w -49   # add other options for ocamlc here

# The list of object files for AAA
AAA_SUBMODULES != echo Aaa__*.ml
AAA_INTERFACES = $(AAA_SUBMODULES:.ml=.mli)

# need this monster for link ordering
AAA_BYTEOBJS != ./linkorder.sh Aaa $(AAA_SUBMODULES) $(AAA_INTERFACES)

Aaa.cma: $(AAA_BYTEOBJS) Aaa.cmo
		ocamlc -o Aaa.cma $(OCAMLFLAGS) $(AAA_BYTEOBJS) Aaa.cmo

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi

.ml.cmo:
		ocamlc $(OCAMLFLAGS) -open Aaa -c $<

Aaa.cmo: Aaa.ml Aaa.cmi
		ocamlc $(OCAMLFLAGS) -c Aaa.ml

.mli.cmi:
		ocamlc $(OCAMLFLAGS) -open Aaa -c $<

Aaa.cmi:
		ocamlc $(OCAMLFLAGS) -c Aaa.mli

# Dependencies
.depend: Makefile $(AAA_INTERFACES) $(AAA_SUBMODULES) Aaa.mli
		ocamldep -map Aaa.mli -open Aaa $(AAA_INTERFACES) $(AAA_SUBMODULES) > .depend

include .depend

.PHONY: clean

clean:
		rm -f *.cma *.cmo *.cmi .depend
