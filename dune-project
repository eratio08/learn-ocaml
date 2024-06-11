(lang dune 3.15)

(generate_opam_files true)

(package
 (allow_empty)
 (name real_world_ocaml)
 (depends ocaml dune base core ppx_jane ppx_let stdio core_unix async core_bench))

