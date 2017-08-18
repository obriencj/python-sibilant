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


import sys as _sys
import fractions as _fractions
import functools as _functools
import operator as _operator

import sibilant as _sibilant
import sibilant.compiler as _compiler
import sibilant.compiler.specials as _specials
import sibilant.compiler.operators as _operators


__all__ = []


def _op(opf, name=None, rename=False):
    name = name if name else opf.__name__

    if rename:
        opf.__name__ = name
        opf.__qualname__ = "sibilant.builtins." + name

    globals()[name] = opf
    __all__.append(name)


def _val(value, name):
    globals()[name] = value
    __all__.append(name)


# === standard operators ===

_op(_operator.or_, "|")
_op(_operator.and_, "&")
_op(_operator.xor, "^")

_op(_operator.delitem, "del-item")
_op(_operator.setitem, "set-item")


# === useful stuff from functools ===

_op(_functools.partial)


# == sibilant data types ===

_op(_sibilant.cons)
_op(_sibilant.car, "car")
_op(_sibilant.setcar, "set-car")
_op(_sibilant.cdr, "cdr")
_op(_sibilant.setcdr, "set-cdr")
_op(_sibilant.is_pair, "pair?")
_op(_sibilant.is_proper, "proper?")
_op(_sibilant.make_proper, "make-proper")
_val(_sibilant.nil, "nil")
_op(_sibilant.is_nil, "nil?")
_val(_sibilant.symbol, "symbol")
_op(_sibilant.is_symbol, "symbol?")
_val(_sibilant.keyword, "keyword")
_op(_sibilant.is_keyword, "keyword?")

_op(_sibilant.first, "first")
_op(_sibilant.second, "second", rename=True)
_op(_sibilant.third, "third", rename=True)
_op(_sibilant.fourth, "fourth", rename=True)
_op(_sibilant.fifth, "fifth", rename=True)
_op(_sibilant.last, "last")

_op(_sibilant.is_undefined, "undefined?")


# === sibilant compiled builtins ===

_val(_compiler.Special, "special")
_op(_compiler.is_special, "special?")

_val(_compiler.Macro, "macro")
_op(_compiler.is_macro, "macro?")

_val(_compiler.Macrolet, "macrolet")
_op(_compiler.is_macrolet, "macrolet?")

_val(_compiler.Operator, "operator")
_op(_compiler.is_operator, "operator?")

from .compiler.specials import *
from .compiler.operators import *

__all__.extend(_specials.__all__)
__all__.extend(_operators.__all__)


# === some python builtin types ===

def _converters():
    _unset = object()
    is_pair = _sibilant.is_pair
    reduce = _functools.reduce

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

    def _apply(fun, arglist=()):
        if is_pair(arglist):
            arglist = arglist.unpack()
        return fun(*arglist)

    _op(_apply, "apply", rename=True)

    map_ = map

    def _map(fun, arglist):
        if is_pair(arglist):
            arglist = arglist.unpack()
        return map_(fun, arglist)

    _op(_map, "map", rename=True)

    zip_ = zip

    def _zip(left, right):
        if is_pair(left):
            left = left.unpack()
        if is_pair(right):
            right = right.unpack()
        return zip_(left, right)

    _op(_map, "zip", rename=True)

    enumerate_ = enumerate

    def _enumerate(value):
        if is_pair(value):
            value = value.unpack()
        return enumerate_(value)

    _op(_enumerate, "enumerate", rename=True)

    reduce_ = reduce

    def _reduce(fun, values, init=_unset):
        if is_pair(values):
            values = values.unpack()
        if init is _unset:
            return reduce_(fun, values)
        else:
            return reduce_(fun, values, init)

    _op(_reduce, "reduce", rename=True)


_converters()

_val(tuple, "tuple")
_op((lambda *vals: vals), "make-tuple", rename=True)
_op((lambda value: isinstance(value, tuple)),
    "tuple?", rename=True)

_val(list, "list")
_op((lambda *vals: list(vals)), "make-list", rename=True)
_op((lambda value: isinstance(value, list)),
    "list?", rename=True)

_val(dict, "dict")
_op((lambda *pairs: dict(pair.unpack() for pair in pairs)),
    "make-dict", rename=True)
_op((lambda value: isinstance(value, dict)),
    "dict?", rename=True)

_val(set, "set")
_op((lambda *vals: set(vals)), "make-set", rename=True)
_op((lambda value: isinstance(value, set)),
    "set?", rename=True)

_op(lambda value: hasattr(value, "__iter__"), "iterable?", rename=True)


# === some python builtin functions ===

_op(callable)
_op(callable, "function?")
_op(next, "next")
_op(format)
_op(getattr)
_op(setattr)
_op(isinstance)
_op(open)
_op(print)
_op(str)
_op(repr)
_op(type)
_op(int)
_op(float)
_op(complex)
_op(range)
_op(help, "help")
_op(dir, "dir")
_op(_fractions.Fraction, "fraction")
_op(_sys.exit, "exit")
_op(__import__, "import")
_op(globals)
_op(locals)

_val(object, "object")
_val(BaseException, "BaseException")
_val(Exception, "Exception")
_val(KeyboardInterrupt, "KeyboardInterrupt")


# === Export 'em ===


__all__ = tuple(__all__)


#
# The end.
