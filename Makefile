.PHONY: clean all EXE copy all_clean

EXE=_build/default/main.exe

all: $(EXE)

$(EXE): *.ml*
	dune build @all --profile release
	@cp $(EXE) ngoc 

test: $(EXE) test.go
	./ngoc --debug test.go
	gcc -g -no-pie test.s -o test
	./test
	go run test.go

export-%:
	cp test.go ../tests/exec/$*.go
	go run test.go > ../tests/exec/$*.out

clean:
	@dune clean

all_clean:
	@find . -type f -name '*.s' -delete
	@find . -type f -name '*.out' -delete
	@find . -type f -name '*.dot' -delete

basic:
	./ngoc ./tst/basic.go --run

factorial:
	./ngoc ./tst/factorial.go --run

arith:
	./ngoc ./tst/arith.go --run

struct:
	./ngoc ./tst/struct.go --run 