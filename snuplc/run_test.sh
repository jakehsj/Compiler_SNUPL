#!/bin/bash

for i in {01..21}; do
    ./reference/test_semanal.relaxed ../test/semanal/test${i}.mod > ../test/semanal/reference/test${i}.mod.out
    dot -Tpdf -o../test/semanal/reference/test${i}.mod.ast.pdf ../test/semanal/reference/test${i}.mod.ast.dot
done