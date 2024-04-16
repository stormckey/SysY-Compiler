# Environment

Tested on docker image debian:latest and ocaml/opam:latest, the latter is recommended where opam is immediately available.
```bash
#debian:latest only
apt install opam
opam init 
eval $(opam env --switch=default)

#install dependencies
#make sure compiler.opam is in . 
cd <dict>
opam install . --deps-only

#compile
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
