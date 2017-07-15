
# Overview of python-sibilant

Sibilant is a dialect of LISP which compiles to [Python] bytecode.


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

It's still a work-in-progress, but it's able to compile nested lambdas
directly into python bytecode


## Model

It's currently convoluted. Some of this can be cut away in the future,
but the result of all the half-hearted poking over all these years is
a bunch of transformations on the input.

* A string or stream representing S-Expressions is parsed into a
  series of events (parse.py)
* The event stream is collected and an ast is formed (ast.py)
* The full ast is simplified into a series of cons cells
* Those cons cells are fed into a combined code/name-space which
  tracks variable scoping and constant values, and collects a series
  of pseudo opcodes (compile.py)
* When the code space is completed, the pseudo ops compile into real
  cpython operations, and a python code is emitted, ready for eval.

I'd like to skip a transform in there somewhere. The ast is nice
because it has line and offset information. However special forms and
runtime defmacro will want to operate on the cons cells. I might be
able to associate the line/offset information with the cons cell
object IDs, and drop the ast entirely. I haven't decided yet.

I really like the pseudop (pseudo opecode) step. It makes it easy to
keep track of only what's important in each distinct operation. It
also allows me to defer proper bytecode emission (and hence worrying
about which minor version of CPython I'm running on). Not getting rid
of that for now.


## Contact

author: Christopher O'Brien  <obriencj@gmail.com>


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
