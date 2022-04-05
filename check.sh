#!/bin/bash
echo "...format *.ml"
ocamlformat -i *.ml 
echo "...build main.native"
ocamlbuild main.native && 
./main.native scan < example.gs 1> /dev/null &&
./main.native parse < example.gs 1> /dev/null &&
./main.native scheck < example.gs 1> /dev/null 

echo "...build test.native"
ocamlbuild -use-ocamlfind -pkgs ounit2 test.native &&
echo "...run tests" && ./test.native