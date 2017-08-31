# This library is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 3 of the
# License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, see
# <http://www.gnu.org/licenses/>.


"""
Pythonic builtin definitions for sibilant.

These are used to bootstrap an importer that can load the
_builtins.lspy.

This module and _builtins are then merged together to create the real
builtins module

This contains re-bindings of common existing Python functions,
sometimes just under a slightly more lisp-ish name, but in some cases
also altered to handle the list form of pairs.

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


def setup(glbls):

    import sys

    from fractions import Fraction as fraction
    from functools import partial, reduce, wraps

    import sibilant
    import sibilant.compiler as compiler
    import sibilant.compiler.tco as tco
    import sibilant.compiler.specials as specials
    import sibilant.compiler.operators as operators

    from sibilant import is_pair


    _all_ = []


    def _op(opf, name=None, rename=False):
        name = name if name else opf.__name__

        if rename:
            opf.__name__ = name
            opf.__qualname__ = "sibilant.builtins." + name

        glbls[name] = opf
        _all_.append(name)


    def _val(value, name):
        glbls[name] = value
        _all_.append(name)


    # === mass imports from other modules ==

    for name, value in specials.__dict__.items():
        glbls[name] = value
        _all_.append(name)

    for name, value in operators.__dict__.items():
        glbls[name] = value
        _all_.append(name)

    # all the exceptions from builtins. We'll pick other values more
    # selectively later
    for name, value in __builtins__.items():
        if isinstance(value, type) and issubclass(value, BaseException):
            glbls[name] = value
            _all_.append(name)


    # == sibilant data types ===

    _op(sibilant.cons, "cons")
    _op(sibilant.car, "car")
    _op(sibilant.setcar, "set-car")
    _op(sibilant.cdr, "cdr")
    _op(sibilant.setcdr, "set-cdr")
    _op(sibilant.is_pair, "pair?")
    _op(sibilant.is_proper, "proper?")
    _op(sibilant.build_proper, "build-proper")
    _val(sibilant.nil, "nil")
    _op(sibilant.is_nil, "nil?")
    _val(sibilant.symbol, "symbol")
    _op(sibilant.is_symbol, "symbol?")
    _val(sibilant.keyword, "keyword")
    _op(sibilant.is_keyword, "keyword?")

    _op(sibilant.copy_pair, "copy-pair")
    _op(sibilant.copy_pair, "join-pairs")
    _op(sibilant.build_unpack_pair, "build-unpack-pair")

    _op(sibilant.first, "first")
    _op(sibilant.second, "second", rename=True)
    _op(sibilant.third, "third", rename=True)
    _op(sibilant.fourth, "fourth", rename=True)
    _op(sibilant.fifth, "fifth", rename=True)
    _op(sibilant.sixth, "sixth", rename=True)
    _op(sibilant.seventh, "seventh", rename=True)
    _op(sibilant.eighth, "eighth", rename=True)
    _op(sibilant.ninth, "ninth", rename=True)
    _op(sibilant.tenth, "tenth", rename=True)

    _op(sibilant.last, "last")


    # === sibilant compiled builtins ===

    _val(compiler.Special, "special")
    _op(compiler.is_special, "special?")

    _val(compiler.Macro, "macro")
    _op(compiler.is_macro, "macro?")

    _val(compiler.Macrolet, "macrolet")
    _op(compiler.is_macrolet, "macrolet?")

    _val(compiler.Operator, "operator")
    _op(compiler.is_operator, "operator?")

    _op(tco.trampoline, "trampoline")
    _op(tco.tailcall, "tailcall")


    # === some python builtin types ===

    _op(partial, "partial")
    _op(wraps, "wraps")


    def _as_tuple(value):
        if is_pair(value):
            return tuple(value.unpack())
        else:
            return tuple(value)

    _op(_as_tuple, "to-tuple", rename=True)


    def _as_list(value):
        if is_pair(value):
            return list(value.unpack())
        else:
            return list(value)

    _op(_as_list, "to-list", rename=True)


    def _as_set(value):
        if is_pair(value):
            return set(value.unpack())
        else:
            return set(value)

    _op(_as_set, "to-set", rename=True)


    def _count(value):
        if is_pair(value):
            return value.count()
        else:
            return len(value)

    _op(_count, "count")


    def _apply(fun, arglist=(), kwargs={}):
        if is_pair(arglist):
            arglist = arglist.unpack()
        return fun(*arglist, **kwargs)

    _op(_apply, "apply", rename=True)


    map_ = map

    @wraps(map_)
    def _map(fun, arglist):
        if is_pair(arglist):
            arglist = arglist.unpack()
        return map_(fun, arglist)

    _op(_map, "map", rename=True)


    zip_ = zip

    @wraps(zip_)
    def _zip(left, right):
        if is_pair(left):
            left = left.unpack()
        if is_pair(right):
            right = right.unpack()
        return zip_(left, right)

    _op(_zip, "zip", rename=True)


    filter_ = filter

    @wraps(filter_)
    def _filter(test, seq):
        if is_pair(seq):
            seq = seq.unpack()
        return filter_(test, seq)

    _op(_filter, "filter", rename=True)


    enumerate_ = enumerate

    def _enumerate(value):
        if is_pair(value):
            value = value.unpack()
        return enumerate_(value)

    _op(_enumerate, "enumerate", rename=True)


    reduce_ = reduce
    unset_ = object()

    def _reduce(fun, values, init=unset_):
        if is_pair(values):
            values = values.unpack()
        if init is unset_:
            return reduce_(fun, values)
        else:
            return reduce_(fun, values, init)

    _op(_reduce, "reduce", rename=True)


    _val(tuple, "tuple")
    _op((lambda *vals: vals), "build-tuple", rename=True)
    _op((lambda value: isinstance(value, tuple)),
        "tuple?", rename=True)

    _op((lambda *vals: vals), "values", rename=True)

    _val(list, "list")
    _op((lambda *vals: list(vals)), "build-list", rename=True)
    _op((lambda value: isinstance(value, list)),
        "list?", rename=True)

    _val(dict, "dict")
    _op((lambda *pairs: dict(pair.unpack() for pair in pairs)),
        "build-dict", rename=True)
    _op((lambda value: isinstance(value, dict)),
        "dict?", rename=True)

    _val(set, "set")
    _op((lambda *vals: set(vals)), "build-set", rename=True)
    _op((lambda value: isinstance(value, set)),
        "set?", rename=True)

    _op(lambda value: hasattr(value, "__iter__"),
        "iterable?", rename=True)


    # === some python builtin functions ===

    _op(callable, "callable?")
    _op(next, "next")
    _op(slice, "slice")
    _op(len, "len")
    _op(format, "format")
    _op(getattr, "getattr")
    _op(setattr, "setattr")
    _op(isinstance, "isinstance")
    _op(open, "open")
    _op(print, "print")
    _op(str, "str")
    _op(repr, "repr")
    _op(type, "type")
    _op(int, "int")
    _op(bool, "bool")
    _op(float, "float")
    _op(complex, "complex")
    _op(fraction, "fraction")

    _op(range, "range")
    _op(help, "help")
    _op(dir, "dir")

    _val(object, "object")

    _op(__import__, "import")
    _op(globals, "globals")
    _op(locals, "locals")

    _op(sys.exit, "exit")


    # done with setup
    return tuple(_all_)


# --- and finally, clean up ---

__all__ = setup(globals())

del setup


#
# The end.
