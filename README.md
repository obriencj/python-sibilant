
# Overview of python-sibilant

Sibilant is a dialect of LISP which compiles to [Python] bytecode.

This was a project was begun in 2007 and subsequently left abaondoned
in early 2008, and I'm putting it up on GitHub as-is for posterity.
There is a strong possibility that I may want to hack on it later.

I believe this project grew out of my hacking with the absurdity that
is [Spexy]. Entirely unlike Spexy, Sibilant had a goal to create a
full-featured, fast, and sane LISP compiler which would emit Python
bytecode.

Unfortunately (as with Spexy) the code was originally kept in CVS on a
host which no longer exists. The `cvsroot` seems to have not been
migrated along as time went by -- or if it did, it moved into a place
where I have been unable to find it.

Whether this code ever gets attention in the future is entirely up in
the air. Perhaps it would be a good project for Python 3 (since most
of my work is tied to Python 2).

If you have any interest in this code, feel free to poke at it. But it
is in a decidedly non-functioning state as of right now. Consider it
akin to a bin of broken or unassembled bits and pieces.

If you want to use a real LISP on Python, I encourage you to direct
your interest towards [Clojure on Python].

[Python]: http://python.org/

[Spexy]: https://github.com/obriencj/python-spexy
"A hackish, LISP-like preprocessor for Python"

[Clojure on Python]: https://github.com/halgari/clojure-py
"An implementation of Clojure in pure Python"


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
