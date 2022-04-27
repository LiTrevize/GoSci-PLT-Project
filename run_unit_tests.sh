echo "...run unit tests" &&
echo "...build test.native" &&
ocamlbuild -use-ocamlfind -pkgs ounit2 test.native &&
./test.native