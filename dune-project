(lang dune 3.14)

(generate_opam_files true)

(package
 (allow_empty)
 (name real_world_ocaml)
 (depends ocaml dune base ppx_jane ppx_let stdio core_unix async core_bench))

