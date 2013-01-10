

minic:depend
	ocamlbuild main.byte -use-menhir
	ln -sf main.byte minic

depend:
	ocamldep main.ml > .depend

clean:
	ocamlbuild -clean
	rm -f minic
	./clean_html.sh

test-parsing:minic
	./tests/runtests.sh parser -parse-only 

test-typing:minic
	./tests/runtests.sh types -type-only

test-exec:minic
	./tests/testexec.sh exec

tests:test-parsing test-typing

all:minic

include .depend


