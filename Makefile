
minic:
	ocamlbuild main.byte -use-menhir
	ln -s main.byte minic

clean:
	ocamlbuild -clean
	rm -f minic


test-parsing:minic
	./tests/runtests.sh parser -parse-only 

test-typing:minic
	./tests/runtests.sh types -type-only

tests:test-parsing test-typing

all:minic


