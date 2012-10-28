
minic:
	ocamlbuild main.byte -use-menhir
	mv main.byte minic

clean:
	rm -r _build
	rm minic


test-parsing:minic
	./tests/runtests.sh parser -parse-only 

tests:test-parsing

all:minic


