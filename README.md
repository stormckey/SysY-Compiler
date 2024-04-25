# Compiler for SysY

The course project for Compiler Principles in Zhejiang University.

Currently only a parser and typechecker is completed.

[Here](https://accsys.pages.zjusct.io/accipit/appendix/sysy-spec/) is the specification for SysY(in Chinese).

## Usage

To run the compiler, you first need to have opam installed. You can also use docker image ocaml/opam:latest.

Then run the following commands:

```bash
#[optional] create a new sandbox
opam switch create sysy ocaml-base-compiler.5.1.1
opam switch set sysy
eval $(opam env)

#install dependencies
cd src
opam install . --deps-only

#compile it
dune build

#Done, the executables are under src/_build/default/bin/lab1.exe and src/_build/default/bin/lab2.exe
```

To run the executable, just type `./lab1.exe <input.sy>`, `./lab2.exe <input.sy>` when executables are in . or run `dune exec lab1 <input.sy>`. If `<input.sy>` is left blank, `test.sy` will be the default.

Other Information

```
opam --version
2.1.5
ocaml --version
The OCaml toplevel, version 5.1.1
```
