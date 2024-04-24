# Compiler for SysY

The course project for Compiler Principles in Zhejiang University.

Currently only a parser and typechecker is completed.

[Here](https://accsys.pages.zjusct.io/accipit/appendix/sysy-spec/) is the specification for SysY(in Chinese).

## Usage

To run the compiler, you first need to have opam installed. You can also use docker image ocaml/opam:latest.

Then run the following commands:

```bash
#install dependencies
opam install . --deps-only

#compile it
dune build

#Done, the executables are under _build/default/src/lab1.exe and _build/default/src/lab2.exe
```

To run the executable, just type `./lab1.exe <input.sy>`, `./lab2.exe <input.sy>` when executables are in . or run `dune exec -- src/lab1.exe <input.sy>`. If `<input.sy>` is left blank, `test.sy` will be the default.

Other Information
```
opam --version              
2.0.10
ocaml --version 
The OCaml toplevel, version 5.1.1
```