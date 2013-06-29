pyle: compile
	@./dist/build/pyle/pyle

pyli: ast_to_lisp compile
	@./dist/build/pyli/pyli | ./ast_to_lisp

parse: compile
	@./dist/build/pyli/pyli

compile:
	@cabal configure > /dev/null
	@cabal build > /dev/null

clean:
	cabal clean
	rm -f sdiff

ast_to_lisp:
	raco exe ast_to_lisp.rkt

