echo "...run integration tests"
echo "...build main.native"
ocamlbuild -pkgs llvm main.native
flag=0
for src in test_cases/*.gs; do
    len=${#src}
    out=${src:0:$len-3}.out
    expect=$(cat $out)
    got=$(./main.native run < $src)
    if [ $expect != $got ]; then
        flag=1
        echo "ERROR in $src: want $expect, got $got"
    fi
done
if [ $flag == 0 ]; then
    echo "PASSED"
else
    echo "FAILED"
fi