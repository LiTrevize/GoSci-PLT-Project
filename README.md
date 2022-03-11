# GoSci-PLT-Project

### Build the GoSci parser

```
ocamlbuild test.native
```

### Run the GoSci parser
```
./test.native
```

### Compiler files
-  `ast.ml`: abstract syntax tree (AST)
-  `scanner.mll`: scanner
-  `gosciparse.mly`: parser

### Other files

- `test.ml`: top-level file to test and run the scanner
- `example.mc`: a sample GoSci source code
- `example.out`: a sample parsed code of example.mc
