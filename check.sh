#!/bin/bash
echo "...format *.ml" &&
ocamlformat -i *.ml &&
echo "...check parser with ocamlyacc" &&
ocamlyacc gosciparse.mly &&
rm gosciparse.ml gosciparse.mli &&
echo "...build main.native" &&
ocamlbuild -pkgs llvm main.native && 
echo "...run main.native on example code" &&
./main.native scan < example.gs 1> /dev/null &&
./main.native parse < example.gs 1> /dev/null &&
./main.native scheck < example.gs 1> /dev/null &&
./main.native irgen < example-ir.gs 1> /dev/null &&

echo "" &&
./run_unit_tests.sh &&

echo "" &&
./run_integration_tests.sh 