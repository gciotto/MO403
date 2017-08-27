#!/bin/bash

set -x

flex -i -o scanner.c scanner.l || exit

gcc -std=c99 -pedantic -o test_scanner *.c || exit

set +x

for i in $(seq 0 $1); do
        test_id=$(printf "%02d" $i)
        ./test_scanner < "tests/prog${test_id}.sl" > "tests/prog${test_id}.out"
        echo "diff tests/prog${test_id}.out tests/tokens${test_id}.res"
        diff "tests/prog${test_id}.out" "tests/tokens${test_id}.res"
done

#rm -f tests/*.out
rm -f test_scanner
