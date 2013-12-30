# Introduction

Compilers are still a bit of a black art. They need not be. Our goals in this project are simple.

* Build a feature-complete compiler for Python 3.
* In such a way that any CS sophomore can read and understand all the code.

To help people understand the codebase, I have written a fair amount of detail about different parts of the codebase as they exist now:

|   |                                                                   |
|:--|:-----------------------------------------------------------------:|
| 1 | [The Parser](http://blog.nullspace.io/obvious-python-parser.html) |


# Usage

pyli is a lexer and parser for (most of) Python 3. Currently you can put Python 3 source into it and it will output a lispy AST:

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
  * decorators

... but it is enough to do some interesting things.

`pyli` is distributed with an MIT license which basically lets you use it for
almost anything, as long as you keep the license in the project, and don't
take my name off of the stuff I wrote.


# LICENSE

Distributed under MIT, which basically means that if you should use this code for anything, you just have to keep a note saying we wrote the code. That said, God help you should you actually decide to use this code.


## MIT License

Copyright (C) [Alex Clemmer](http://nullspace.io/) ([@hausdorff](https://github.com/hausdorff))

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
