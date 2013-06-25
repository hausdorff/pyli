pyle:
	@./dist/build/pyle/pyle

pyli:
	@./dist/build/pyli/pyli | ./ast_to_lisp

compile:
	@cabal configure > /dev/null
	@cabal build > /dev/null

clean:
	cabal clean
	rm -f sdiff
