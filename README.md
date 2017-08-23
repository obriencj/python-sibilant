# Overview of python-sibilant

[![Build Status](https://travis-ci.org/obriencj/python-sibilant.svg?branch=master)](https://travis-ci.org/obriencj/python-sibilant)

Sibilant is a dialect of LISP which compiles to [Python] bytecode.

Sibilant is not done. It's still being organically grown in bars and
coffee shops whenever I get a chance to sit alone. I will eventually
get to the point where it seems like a 1.0.0 is sane. Until then, this
is version 0.9.0 and every commit or pull-request could introduce
dramatic changes.

I suspect that 1.0.0 will have approximately one user (myself). The
purpose of version 1 will be to gather my own feedback on how I use
sibilant, how it falters and fails, how it shines, etc. Then one day,
version 2.0.0 will happen, and that's going to be some good stuff!


## But Why?

Mostly "why not" and "I do what I want"

But also because I really love the idea of domain-specific languages.
I wanted defmacro in Python so bad! So I made defmacro in Python to
let me write some other things a little easier. The worst kind of
lazy.


## Origins

This was a project begun in 2007 and subsequently left abaondoned in
early 2008, and I'm putting it up on GitHub as-is for posterity.
There is a strong possibility that I may want to hack on it later.

I believe this project grew out of my hacking with the absurdity that
is [Spexy]. Entirely unlike Spexy, Sibilant had a goal to create a
full-featured, sane-ish LISP compiler which would emit Python bytecode.

Unfortunately (as with Spexy) the code was originally kept in CVS on a
host which no longer exists. The `cvsroot` seems to have not been
migrated along as time went by -- or if it did, it moved into a place
where I have been unable to find it.

[Python]: http://python.org/

[Spexy]: https://github.com/obriencj/python-spexy
"A hackish, LISP-like preprocessor for Python"


## Reborn

Some time in 2014 I started poking half-heartedly at this code
again. For the next three years I made pathetic incremental changes
that went mostly nowhere. I wrote unit tests, wrote types to mimic
cons cells and symbols. I poked around with the idea of a trampoline
and continuation-passing style.

Then suddenly in July of 2017 I went nuts and threw together the
compiler in a week while drinking at a barcade.


## Python Version Support

CPython 3.5 and 3.6 are currently supported. Sibilant outputs python
bytecode directly, and creates code, function, and module instances
from there. The Python 3 line has changed its bytecode quite a bit
between these two minor versions. It's possible that earlier versions
of Python 3 could also be supported, but there are some syntax
features that the sibilant implementation uses which would need to be
changed (such as import of a tuple names, etc).


## References

I rely heavily on the auto-generated documentation for the `dis`
module.

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


### Line Numbers

A big advantage of sibilant over an interpreted lisp using a lambda
emitter in Python is that the bytecode sibilant emits can have a
line-number-table associated with it. This means that exceptions or
tracebacks will interleave between python source code and sibilant
source code, and correctly show the line that the raise came from.


### defmacro and defmacrolet

A special form and macro system is implemented already. Macros are the
simple, low-level variety, transforming the `cons` list from the
parsed source code and emitting a new list representing the expanded
form. An implementation of `macroexpand-1` is included for macro
debugging purposes.


### try/except/else/finally as an Expression

The `try` special form can be used as an expression, evaluating to the
block that runs last. That's like, my favorite feature.


### Future Feature: Generators

Sibilant doesn't currently offer a way to create generators. It is
definitely on the horizon. There's a few complicated features that
need work ahead of time, such as a loop construct that will correctly
function with nested scopes but which can be optimized when used
inside of a single scope.


### Future Feature: List Comprehensions

Definitely need a special form for emitting a list comprehension
and/or a generator expression.


### Future Feature: Compile to file

Right now sibilant compiles to bytecode in-memory, and creates
function instances from there. I'd like to be able to use the importer
and/or compileall system to convery sibilant sources directly into
.pyc files. These would of course still have a hard dependency on
sibilant, in the most minimal case if only for the `Symbol` and `Pair`
datatypes and associated functions.


### Future Feature: Rewrite Sibilant in Sibilant

I'd like to get to the point where I can rewrite the compiler
subpackage in sibilant itself. Then compile the new compiler in the
old compiler, and finally re-compile the new compiler using itself.


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
