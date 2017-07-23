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

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


import sys as _sys
import fractions as _fractions
import functools as _functools
import operator as _operator
import sibilant as _sibilant
import sibilant.compiler as _compiler


__all__ = []


def _reduce_op(opf, name=None):
    # in the future, this can become a special. Ops invoked with two
    # arguments can result in the normal call. Ops invoked with more
    # than two arguments can be wrapped in a reduce call.

    reduce = _functools.reduce

    def fun(*args):
        return reduce(opf, args)

    name = name if name else opf.__name__

    fun.__name__ = opf.__name__
    fun.__qualname__ = opf.__name__
    fun.__doc__ = opf.__doc__
    fun.__symbol__ = _sibilant.symbol(name)

    globals()[name] = fun
    __all__.append(name)


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


def _and(val1, *valn):
    for val2 in valn:
        val1 = val1 and val2
        if not val1:
            break
    return val1


def _or(val1, *valn):
    for val2 in valn:
        val1 = val1 or val2
        if val1:
            break
    return val1


# === standard operators ===

_op(_operator.add, "+")
_op(_operator.sub, "-")
_op(_operator.mul, "*")

_op(_operator.pos, "pos")
_op(_operator.neg, "neg")

_op(_and, "and", rename=True)
_op(_or, "or", rename=True)
_op((lambda val: not val), "not", rename=True)

_op(_operator.pow, "**")
_op(_operator.truediv, "/")
_op(_operator.mod, "%")
_op(_operator.floordiv, "//")
_op(_operator.or_, "|")
_op(_operator.and_, "&")
_op(_operator.xor, "^")
_op(_operator.invert, "~")

_op(_operator.contains, "in")
_op(_operator.is_, "is")
_op(_operator.is_not, "is-not")
_op(_operator.eq, "eq")
_op(_operator.eq, "==")
_op(_operator.ne, "ne")
_op(_operator.ne, "!=")
_op(_operator.ge, ">=")
_op(_operator.gt, ">")
_op(_operator.le, "<=")
_op(_operator.lt, ">")

_op((lambda obj, key: obj[key]), "item")
_op(_operator.delitem, "del-item")
_op(_operator.setitem, "set-item")

# === useful stuff from functools ===

_op(_functools.partial)
_op(_functools.reduce)


# == sibilant data types ===

_op(_sibilant.cons)
_op(_sibilant.car, "car")
_op(_sibilant.setcar, "set-car")
_op(_sibilant.cdr, "cdr")
_op(_sibilant.setcdr, "set-cdr")
_op(_sibilant.ref)
_op(_sibilant.attr)
_op(_sibilant.deref)
_op(_sibilant.setref)
_op(_sibilant.is_pair, "pair?")
_op(_sibilant.is_proper, "proper?")
_op(_sibilant.make_proper, "make-proper")
_val(_sibilant.nil, "nil")
_op(_sibilant.is_nil, "nil?")
_val(_sibilant.symbol, "symbol")
_op(_sibilant.is_symbol, "symbol?")

_op(_sibilant.first, "first")
_op(_sibilant.second, "second", rename=True)
_op(_sibilant.third, "third", rename=True)
_op(_sibilant.fourth, "fourth", rename=True)
_op(_sibilant.fifth, "fifth", rename=True)
_op(_sibilant.last, "last")

_op(_sibilant.is_undefined, "undefined?")


# === compiler special forms ===

_val(_compiler.Special, "special")
_op(_compiler.is_special, "special?")

_val(_compiler.Macro, "macro")
_op(_compiler.is_macro, "macro?")


def _specials():
    for name, special in _compiler.builtin_specials():
        globals()[name] = special
        __all__.append(name)


_specials()


# === some python builtin types ===


def _converters():
    is_pair = _sibilant.is_pair

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

    def _apply(fun, arglist):
        if is_pair(arglist):
            arglist = arglist.unpack()
        return fun(*arglist)

    _op(_apply, "apply", rename=True)


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
_op((lambda value: isinstance(value, dict)))

_val(set, "set")
_op((lambda *vals: set(vals)), "make-set", rename=True)
_op((lambda value: isinstance(value, set)),
    "set?", rename=True)


# === some python builtin functions ===

_op(callable)
_op(callable, "function?")
_op(format)
_op(getattr)
_op(isinstance)
_op(open)
_op(print)
_op(setattr)
_op(str)
_op(repr)
_op(type)
_op(int)
_op(float)
_op(complex)
_op(_fractions.Fraction, "fraction")
_op(_sys.exit, "exit")
_op(__import__, "import")


# === misc ===

# TODO: this needs to become a special, and should emit the raise
# bytecode.
def _raise(exc):
    raise exc

_op(_raise, "raise", rename=True)


__all__ = tuple(__all__)


#
# The end.
