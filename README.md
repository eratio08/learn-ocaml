# Learning OCaml

Working through [Real World OCaml](https://dev.realworldocaml.org/).

## Setup

Create a local switch and install dependencies by running

```shell
opam switch create . 5.1.1
opam install . --dep-only --yes --with-doc
```

The switch functions as a scope for dependencies.
Opam will install dependencies in `real_world_ocampl.opam`.
This file is generated base on `dune-project` whenever `dune build` us ran.
