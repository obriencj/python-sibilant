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
builtin definitions for sibilant. These are made available in the
scope of all loaded modules.

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


import sys as _sys
import operator as _operator
import sibilant as _sibilant
import sibilant.compiler as _compiler


__all__ = []


def _reduce_op(opf, name=None):
    # in the future, this can become a special. Ops invoked with two
    # arguments can result in the normal call. Ops invoked with more
    # than two arguments can be wrapped in a reduce call.

    from functools import reduce

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


_reduce_op(_operator.add, "+")
_reduce_op(_operator.sub, "-")
_reduce_op(_operator.mul, "*")

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

_op((lambda fun, args: fun(*args)), "apply", rename=True)

_op((lambda *vals: _sibilant.nil if not vals else
     _sibilant.cons(*vals, _sibilant.nil)),
    "make-list", rename=True)

_op((lambda *vals: vals), "make-py-tuple", rename=True)
_op((lambda *vals: list(vals)), "make-py-list", rename=True)
_op((lambda value: tuple(value)), "py-tuple", rename=True)
_op((lambda value: list(value)), "py-list", rename=True)
_op((lambda value: isinstance(value, list)),
    "py-list?", rename=True)
_op((lambda value: isinstance(value, tuple)),
    "py-tuple?", rename=True)

_op(_sibilant.cons)
_op(_sibilant.car)
_op(_sibilant.cdr)
_op(_sibilant.ref)
_op(_sibilant.attr)
_op(_sibilant.deref)
_op(_sibilant.setref)

_op(_sibilant.is_pair, "pair?")
_op(_sibilant.is_list, "list?")

_val(_sibilant.nil, "nil")
_op(_sibilant.is_nil, "nil?")

_val(_sibilant.symbol, "symbol")
_op(_sibilant.is_symbol, "symbol?")

_val(_sibilant.undefined, "undefined")
_op(_sibilant.is_undefined, "undefined?")

_val(_compiler.Macro, "macro")
_op(_compiler.is_macro, "macro?")

_op(callable)
_op(callable, "function?")
_op(format)
_op(getattr)
_op(isinstance)
_op(open)
_op(print)
_op(setattr)
_op(_sys.exit, "exit")


__all__ = tuple(__all__)


#
# The end.
