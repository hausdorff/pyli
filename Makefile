lex:
	@./dist/build/pyle/pyle

compile:
	@cabal configure > /dev/null
	@cabal build > /dev/null
