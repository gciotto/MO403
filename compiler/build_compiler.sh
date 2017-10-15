#!/bin/bash

set -x

bison -d -o parser.c $1  || exit

flex -i -o scanner.c $2 || exit

gcc -std=c99 -pedantic -o compiler *.c || exit

set +x

for i in $(seq 0 $3); do
        test_id=$(printf "%02d" $i)
        ./test_tree < "tests/prog${test_id}.sl" > "tests/prog${test_id}.out"
        echo "diff tests/prog${test_id}.out" "tests/result${test_id}"
        diff "tests/prog${test_id}.out" "tests/result${test_id}"
done

rm -f tests/*.out
rm -f test_parser
