for i in {01..21}; do
    formatted_i=$(printf "%02d" $i) 
    rm -f ../test/semanal/test${formatted_i}.mod.out
    rm -f ../test/semanal/test${formatted_i}.mod.ast.dot
    rm -f ../test/semanal/test${formatted_i}.mod.ast.pdf
done
