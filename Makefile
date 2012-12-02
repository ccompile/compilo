
minic:clean
	ocamlbuild main.byte -use-menhir
	mv main.byte minic
	firefox http://antonin.delpeuch.eu/tmp/play.html

clean:
	rm -rf _build
	rm -f minic


test-parsing:minic
	./tests/runtests.sh parser -parse-only 

test-typing:minic
	./tests/runtests.sh types -type-only

tests:test-parsing test-typing

all:minic


