#!/bin/bash

set -x

flex -i -o scanner.c scanner.l

gcc -std=c99 -pedantic -o test_scanner *.c

for i in $(seq 0 $1); do
        ./test_scanner < "tests/prog0$i.sl" > "tests/prog0$i.out"
        diff "tests/prog0$i.out" "tests/tokens0$i.res"
done

rm -f tests/*.out
rm -f test_scanner
