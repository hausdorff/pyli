pyli is a ~~lexer~~ compiler for Python 3. Currently you can put Python 3 source into it and it will output a lispy AST:

    cat stuff.py | make pyli

Or more verbosely:

    cabal configure
    cabal build
    cat stuff.py | ./dist/build/pyli/pyli

The lexer is both built directly into the `pyli` compiler, and compiled as a separate binary just for funsies. You can run it as:

    cat stuff.py | make pyle

Or more verbosely:

    cabal configure
    cabal build
    cat stuff.py | ./dist/build/pyle/pyle

`pyli` is pretty bare-bones. We do not currently support:

  * unicode (I know, I know)
  * bytestrings
  * unicode bytestrings!
  * tabs, because they're evil. (just kidding; it's because I'm lazy)

... but it is enough to do some interesting things.

`pyli` is distributed with an MIT license which basically lets you use it for
almost anything, as long as you keep the license in the project, and don't
take my name off of the stuff I wrote.