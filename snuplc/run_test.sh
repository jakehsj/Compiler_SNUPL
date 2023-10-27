#!/bin/bash

for i in {01..10}; do
    ./test_parser ../test/parser/test${i}.mod > ../test/parser/test${i}.mod.out
    dot -Tpdf -o../test/parser/test${i}.mod.ast.pdf ../test/parser/test${i}.mod.ast.dot
done
