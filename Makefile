
minic:
	ocamlbuild main.byte -use-menhir
	mv main.byte minic

clean:
	rm -r _build
	rm minic


test-parsing:
	./tests/runtests.sh prof/syntax/good -parse-only 0
	./tests/runtests.sh prof/syntax/bad -parse-only 1
	./tests/runtests.sh prof/exec -parse-only 0
	./tests/runtests.sh prof/exec-fail -parse-only 0	
	./tests/runtests.sh prof/typing/bad -parse-only 0	
	./tests/runtests.sh prof/typing/good -parse-only 0	

tests:test-parsing

all:minic


