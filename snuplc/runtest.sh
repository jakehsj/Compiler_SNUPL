#!/bin/bash

# 첫 번째 인자를 받음
test_number=$1

# 인자가 주어진 경우 해당 테스트만 실행
if [ ! -z "$test_number" ]; then
    formatted_i=$(printf "%02d" $test_number)
    ./test_semanal ../test/semanal/test${formatted_i}.mod > ../test/semanal/test${formatted_i}.mod.out
    dot -Tpdf -o../test/semanal/test${formatted_i}.mod.ast.pdf ../test/semanal/test${formatted_i}.mod.ast.dot
else
    # 인자가 주어지지 않은 경우 모든 테스트를 실행
    for i in {01..21}; do
        formatted_i=$(printf "%02d" $i)
        ./test_semanal ../test/semanal/test${formatted_i}.mod > ../test/semanal/test${formatted_i}.mod.out
        dot -Tpdf -o../test/semanal/test${formatted_i}.mod.ast.pdf ../test/semanal/test${formatted_i}.mod.ast.dot
    done
fi
