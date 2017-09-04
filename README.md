# Overview of python-sibilant

[![Build Status](https://travis-ci.org/obriencj/python-sibilant.svg?branch=master)](https://travis-ci.org/obriencj/python-sibilant)

Sibilant is a dialect of [LISP] which compiles to [Python] bytecode.

Sibilant is not done. It's still being organically grown in bars and
coffee shops whenever I get a chance to sit alone. I will eventually
get to the point where it seems like a 1.0.0 is sane. Until then, this
is version 0.9.0 and every commit or pull-request could introduce
dramatic changes.

[LISP]: https://en.wikipedia.org/wiki/Lisp_(programming_language)

[Python]: http://python.org/


## But Why?

Mostly "why not" and "I do what I want"

But also because I really love the idea of domain-specific languages.
I wanted defmacro in Python so bad! So I made defmacro in Python to
let me write some other things a little easier. The worst kind of
lazy.


## Origins

This was a project begun in 2007 and subsequently left abaondoned in
early 2008,.

I believe the concept grew out of my hacking with the absurdity that
is [Spexy]. But while Spexy was mostly a joke and a puzzle, Sibilant
had a goal to create a working and sane-ish LISP compiler.

I wasn't ready for such an undertaking, and a great deal of Real Life
took up my time instead. The modest first passes as Sibilant sat
mostly forgotten in a CVS repo.

[Spexy]: https://github.com/obriencj/python-spexy
"A hackish, LISP-like preprocessor for Python"

Some time in January of 2014 I ran across my checkout. The upstream
`cvsroot` had disappeared -- it was on a host that had probably been
migrated, and I'd not bothered to bring along these repos, or I did a
really good job hiding it from myself. I imported what I had left to
GitHub for posterity, unsure of the fate of the project.

For the next few years I would idly poke at bits here and there. I
toyed with different ideas for some of the basic data types, but never
got anywhere serious.

Then suddenly in July of 2017 I went nuts and threw together the
compiler in a week while drinking at a barcade.

It has been a fantastic learning experience, and excellent mental
exercise. While the Sibilant user base might always number in just the
single digits, I could never consider the project a failure. The sheer
joy from when a particular feature comes to life for the first time,
or the satisfaction of seeing the language develop are enough. I am
excited to continue hacking at it.


## Python Version Support

CPython 3.5 and 3.6 are currently supported. Sibilant outputs python
bytecode directly, and creates code, function, and module instances
from there. The Python 3 line has changed its bytecode quite a bit
between these two minor versions. It's possible that earlier versions
of Python 3 could also be supported, but there are some syntax
features that the sibilant implementation uses which would need to be
changed. It's a low priority currently, but I'm definitely open to
pull requests on that front if someone really needs 3.4 support.


## References

Sibilant targets Python bytecode directly. I rely heavily on the
auto-generated documentation for the `dis` module.

* [Module `dis` in Python 3.6](https://docs.python.org/3.6/library/dis.html)
* [Module `dis` in Python 3.5](https://docs.python.org/3.5/library/dis.html)


## Features

Sibilant is slowly growing as more special forms and macros are
added. Below are a few of the key features


### Importer

Python 3 provides an extensible import system via `importlib`. When
the `sibilant.importer` module is loaded, this system will be extended
to support treating sibilant source files (files found in `sys.path`
and ending in `.lspy` or `.sibilant`) as packages or modules.

In other words, to enable loading of sibilant code at runtime, you
just need to have `import sibilant.importer` at the beginning of your
top-level module.

From within a sibilant module the `import` function allows fetching a
module from the importer. `defimport` and `defimportfrom` will bind
modules or their contents to the global namespace.


### Compile to File

Sibilant has the ability to compile a `.lspy` file into a `.pyc` which
can then be loaded by the default importer. Modules loaded in this
manner skip the parse and compile stages, but still execute in-order
during load. These modules have a hard dependency on sibilant -- they
are not stand-alone. All of the sibilant types are pulled in
dynamically.


### Line Numbers

A big advantage of sibilant over an interpreted lisp using a lambda
emitter in Python is that the bytecode sibilant emits can have a
line-number-table associated with it. This means that exceptions or
tracebacks will interleave between python source code and sibilant
source code, and correctly show the line that the raise came from.


### Tail-call optimization

Sibilant produces functions wrapped in a simple trampoline. Calls made
in a tail position will result in a trampoline bounce to avoid eating
up stack space. TCO does have the somewhat frustrating side-effect of
collapsing the call stack, which can make tracebacks difficult to
debug.  It is possible to disable TCO during compilation, and it is
also possible to mark specific functions as invalid for TCO (such as
locals and globals, which rely on observing their calling frame to
provide their return value)


### Parse-time Macros: reader macros

Parse time macros defined via `set-macro-character` can transform
the source stream before it becomes a source object


### Compile-time Macros: defmacro

Compile time macros defined via defmacro are the simple, low-level
variety, transforming the `cons` list from the parsed source code and
emitting a new list representing the expanded form. An implementation
of `macroexpand-1` is included for macro debugging purposes.


### Compile-time optimized operators

The common Python operators and comparators are implemented such that
they have both a compile-time and run-time representation. Where
possible, operators will compile to direct bytecode operations, but
can also be passed and called as runtime functions.


### try/except/else/finally

The `try` special form can be used as an expression, evaluating to the
block that runs last.


### Future Feature: Generators

Sibilant doesn't currently offer a way to create generators. It is
definitely on the horizon. There's a few complicated features that
need work ahead of time, such as a loop construct that will correctly
function with nested scopes but which can be optimized when used
inside of a single scope.


### Future Feature: List Comprehensions

Definitely need a special form for emitting a list comprehension
and/or a generator expression.


### Future Feature: Rewrite Sibilant in Sibilant

I'd like to get to the point where I can rewrite the compiler
subpackage in sibilant itself. Then compile the new compiler in the
old compiler, and finally re-compile the new compiler using itself.

The sibilant compiler will eventually become sibilantzero, to be
relegated to a simple build dependency in producing sibilant proper.


## Contact

author: Christopher O'Brien  <obriencj@gmail.com>

original git repository: <https://github.com/obriencj/python-sibilant>


## License

This library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 3 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, see
<http://www.gnu.org/licenses/>.
