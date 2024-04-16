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

Other Information
```
opam --version              
2.0.10
ocaml --version 
The OCaml toplevel, version 5.1.1
```
