(lang dune 3.6)

(name TuringMachine)

(generate_opam_files true)

(source
 (github toasted-cornkernels/TuringMachine))

(authors "ToastedCornkernels")

(maintainers "ToastedCornkernels")

(license LICENSE)

(package
 (name TuringMachine)
 (synopsis "A Turing machine simulator.")
 (description "A Turing machine simulator.")
 (depends ocaml dune utop ocamlgraph yojson))
