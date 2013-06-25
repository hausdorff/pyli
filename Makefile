lex:
	@./dist/build/pyle/pyle

parse:
	@./dist/build/pyli/pyli

compile:
	@cabal configure > /dev/null
	@cabal build > /dev/null

clean:
	cabal clean
	rm -f sdiff
