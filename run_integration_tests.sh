echo "...run integration tests"
echo "...build main.native"
ocamlbuild -pkgs llvm main.native
flag=0
pass=0
fail=0
for src in test_cases/*.gs; do
    len=${#src}
    out=${src:0:$len-3}.out
    printf "%-60s\r" "running ${src:0:$len-3}"
    expect=$(cat $out)
    got=$(./main.native run < $src)
    if [ "$expect" != "$got" ]; then
        flag=1
        fail=`expr $fail + 1`
        echo "ERROR in $src: want $expect, got $got"
    else
        pass=`expr $pass + 1`
    fi
done
echo ""
if [ $flag == 0 ]; then
    echo "${pass} cases PASSED"
else
    echo "${fail} cases FAILED (${pass} passed)"
fi