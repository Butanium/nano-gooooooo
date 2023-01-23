#!/bin/sh

GO_SRC=$(find test/*.go)

rm compare_out*.txt
echo "starting tests..."
for SRC in $GO_SRC; do
  NAME=$(echo $SRC | cut -d '.' -f 1 | cut -d '/' -f 2)
  echo "____Fichier $NAME ___\n" >> compare_out1.txt
  echo "____Fichier $NAME ___\n" >> compare_out2.txt
  go run "test/$NAME.go" >> compare_out1.txt
  echo "">> compare_out1.txt
  ./ngoc "test/$NAME.go" && gcc -no-pie "test/$NAME.s" -o "test/$NAME.out" && ./test/$NAME.out  >> compare_out2.txt
  echo "" >> compare_out2.txt
  echo "program $NAME done"
done
echo "test done"
diff --color compare_out1.txt compare_out2.txt

rm test/*.s test/*.out