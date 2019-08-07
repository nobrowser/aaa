(lang dune 1.11)
(generate_opam_files true)

(license "ZPL-2.1")
(authors "Ian Zimmerman <itz@no-use.mooo.com>")
(maintainers "Ian Zimmerman <itz@no-use.mooo.com>")
(version "0.0.8~dev")
(source
 (uri "git+file:///home/itz/git/aaa#master"))

(package
 (name aaa)
 (synopsis "Tiny batteries")
 (depends
  (ocaml (>= 4.07))
  (ocamlfind (>= 1.8.1))
  (qcheck-core (>= 0.9))))
