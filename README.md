# GoSci-PLT-Project

## Prerequisite
| Tool | Installation |
| --- | --- |
| opam | `apt install opam` |
| llvm | `apt install llvm` |
| gcc | `apt install gcc` |


## Test the GoSci compiler
### Unit Test
Install ounit2 package:
```
opam install ounit2
```

Install llvm package:
```
opam install llvm
```

Build test code:
```
ocamlbuild -use-ocamlfind -pkgs ounit2 test.native
```

Run all test cases:
```
./test.native
```



## Run the GoSci compiler
Build the GoSci compiler
```
ocamlbuild -pkgs llvm main.native
```

Run the GoSci compiler

```
./main.native <action>
```
And `action` can be:
- `scan`: generate sequence of tokens
- `parse`: generate the abstract syntax tree (AST)
- `scheck`: generate a semantically-checked AST
- `irgen`: generate LLVM code
- `codegen`: generate assembly code
- `compile`: generate executable
- `run`: compile then execute the program

### Compiler files
-  `ast.ml`: abstract syntax tree (AST)
-  `scanner.mll`: scanner
-  `gosciparse.mly`: parser
-  `sast.ml`: semantically-checked AST (sAST)
-  `semant.ml`: turn an AST to an sAST
-  `irgen.ml`: turn sAST to llvm module
-  `main.ml`: top-level file to run the compiler
-  `test.ml`: test cases for unit tests

### Other files

- `example.gs`: a sample GoSci source code

## Format Source Code
Install Dependency
```
opam install ocamlformat
```

Format the source file and replace with corrected versions, detailed instruction can go to [Dune's manual](https://dune.readthedocs.io/en/stable/formatting.html#formatting-a-project).
```
dune build @fmt --auto-promote
```
