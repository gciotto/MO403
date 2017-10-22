#!/bin/bash

set -x

bison -d -o parser.c $1  || exit

flex -i -o scanner.c $2 || exit

gcc -g -std=c99 -pedantic -o compiler *.c || exit

set +x

for i in $(seq 0 $3); do
        test_id=$(printf "%02d" $i)
        ./compiler < "tests/prog${test_id}.sl" > "tests/prog${test_id}.out"
        echo "diff tests/prog${test_id}.out" "tests/prog${test_id}.mep"
        diff tests/prog${test_id}.out tests/prog${test_id}.mep
        # ./mepa/mepa.py --silent --limit 12000 --progfile tests/prog${test_id}.out < tests/data${test_id}.in > tests/prog${test_id}_exec.out
        # cat tests/prog${test_id}_exec.out
done

rm -f tests/*.out
# rm -f compiler
