
minic:clean
	ocamlbuild main.byte -use-menhir
	mv main.byte minic

clean:
	rm -rf _build
	rm -f minic


test-parsing:minic
	./tests/runtests.sh parser -parse-only 
	./tests/runtests.sh types -type-only

tests:test-parsing

all:minic


