# GoSci-PLT-Project

## Prerequisite
| Tool | Installation |
| --- | --- |
| opam | `apt install opam` |
| llvm | `apt install llvm` |
| gcc | `apt install gcc` |

Install llvm package for OCaml:
```
opam install llvm
```

## Format Source Code
Install Dependency
```
opam install ocamlformat
```

Format the source file and replace with corrected versions, detailed instruction can go to [Dune's manual](https://dune.readthedocs.io/en/stable/formatting.html#formatting-a-project).
```
dune build @fmt --auto-promote
```


## Test the GoSci compiler
### Unit Test
Install ounit2 package:
```
opam install ounit2
```

Build test code:
```
ocamlbuild -use-ocamlfind -pkgs ounit2 test.native
```

Run all test cases:
```
./test.native
```

Or simply run:
```
./run_unit_tests.sh
```

### Integration Test

All test cases are in directory `test_cases`, where `*.gs` are the source programs and `*.out` are the expected output. To run all test cases:
```
./run_integration_tests.sh
```



## Run the GoSci compiler
Build the GoSci compiler
```
ocamlbuild -pkgs llvm main.native
```

Run the GoSci compiler

```
./main.native <action> [outfile]
```
And `action` can be:
- `scan`: generate sequence of tokens
- `parse`: generate the abstract syntax tree (AST)
- `scheck`: generate a semantically-checked AST
- `irgen`: generate LLVM code
- `codegen`: generate assembly code `<outfile>.s`
- `compile`: generate executable with filename `outfile`
- `run`: compile then execute the program

### Compiler files
-  `ast.ml`: abstract syntax tree (AST)
-  `scanner.mll`: scanner
-  `gosciparse.mly`: parser
-  `sast.ml`: semantically-checked AST (sAST)
-  `semant.ml`: turn an AST to an sAST
-  `irgen.ml`: turn sAST to llvm module
-  `main.ml`: top-level file to run the compiler

### Test files
-  `test.ml`: test cases for unit tests
-  `run_unit_tests.sh`: shell script to run all unit tests
-  `test_cases/*`: test cases for integration tests
-  `run_integration_tests.sh`: shell script to run all integration tests
-  `check.sh`: format the code and run all the tests

### Other files

- `example.gs`: a sample GoSci code of all usage
- `example_demo.gs`: a sample GoSci code of a physics simulation
