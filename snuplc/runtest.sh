#!/bin/bash

for i in {01..21}; do
    formatted_i=$(printf "%02d" $i)
    ./test_semanal ../test/semanal/test${formatted_i}.mod > ../test/semanal/test${formatted_i}.mod.out
    dot -Tpdf -o../test/semanal/test${formatted_i}.mod.ast.pdf ../test/semanal/test${formatted_i}.mod.ast.dot
done