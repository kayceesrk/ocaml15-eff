# ocaml15-eff
Examples for Algebraic Effects talk at OCaml'15 Workshop. Needs OCaml with
algebraic effects: https://github.com/kayceesrk/ocaml/tree/effects.

## Installing OCaml-effects compiler

  1. Install [opam-compiler-conf](https://github.com/gasche/opam-compiler-conf)
  2. Get the OCaml-effects compiler

		$ git clone https://github.com/ocamllabs/ocaml-effects
		$ cd ocaml-effects
		$ git checkout effects-native
		$ opam compiler-conf configure
		$ make world.opt
		$ opam compiler-conf install

  3. Use `opam switch` to switch to the newly installed compiler
