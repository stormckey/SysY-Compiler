# Environment

```bash
#install dependencies
#make sure compiler.opam is in . 
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

`ocaml/opam:latest` image is compatible with this project.