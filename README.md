# Learning OCaml

Working through [Real World OCaml](https://dev.realworldocaml.org/).

## Setup

Create a local switch

```shell
opam switch create . --deps-only --with-test --with-doc
eval $(opam env)
opam install core core_bench utop
```
