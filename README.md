# GoSci-PLT-Project

### Test the GoSci compiler
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

### Build the GoSci compiler

```
ocamlbuild main.native
```

### Run the GoSci compiler

Scan only:
```
./main.native scan
```

Scan then parse
```
./main.native parse
```

Scan, parse, then semantic check
```
./main.native scheck
```

### Compiler files
-  `ast.ml`: abstract syntax tree (AST)
-  `scanner.mll`: scanner
-  `gosciparse.mly`: parser

### Other files

- `main.ml`: top-level file to test and run the compiler
- `example.gs`: a sample GoSci source code
- `test.ml`: all test cases