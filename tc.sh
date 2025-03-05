cool --parse $1
ocamlc -w -A main.ml
./a.out "$1"-ast
cool --type $1 --out ref