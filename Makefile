
minic:clean
	ocamlbuild main.byte -use-menhir
	ln -s main.byte minic

clean:
	ocamlbuild -clean
	rm -f minic
	./clean_html.sh

test-parsing:minic
	./tests/runtests.sh parser -parse-only 

test-typing:minic
	./tests/runtests.sh types -type-only

test-exec:minic
	./tests/runtests.sh exec

tests:test-parsing test-typing

all:minic


