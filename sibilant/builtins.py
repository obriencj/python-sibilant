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


import operator
import sibilant
import sibilant.compiler

from functools import reduce


__all__ = []


def _reduce_op(opf, name=None):
    # in the future, this can become a special. Ops invoked with two
    # arguments can result in the normal call. Ops invoked with more
    # than two arguments can be wrapped in a reduce call.

    def fun(*a):
        return reduce(opf, a)

    name = name if name else opf.__name__

    fun.__name__ = opf.__name__
    fun.__qualname__ = opf.__name__
    fun.__doc__ = opf.__doc__
    fun.__symbol__ = sibilant.symbol(name)

    globals()[name] = fun
    __all__.append(name)


def _op(opf, name=None):
    #def fun(*a):
    #    return opf(*a)

    name = name if name else opf.__name__
    sym = sibilant.symbol(name)

    #fun.__name__ = name
    #fun.__doc__ = opf.__doc__
    #fun.__symbol__ = symbol(name)

    globals()[name] = opf  # fun
    __all__.append(name)


def _val(value, name):
    globals()[name] = value
    __all__.append(name)


_reduce_op(operator.add, "+")
_reduce_op(operator.sub, "-")
_reduce_op(operator.mul, "*")

_op(operator.pow, "**")
_op(operator.truediv, "/")
_op(operator.mod, "%")
_op(operator.floordiv, "//")
_op(operator.or_, "|")
_op(operator.and_, "&")
_op(operator.xor, "^")
_op(operator.invert, "~")

_op(sibilant.cons)
_op(sibilant.car)
_op(sibilant.cdr)
_op(sibilant.ref)
_op(sibilant.attr)
_op(sibilant.deref)
_op(sibilant.setref)

_val(sibilant.nil, "nil")
_val(sibilant.symbol, "symbol")
_val(sibilant.constype, "constype")
_val(sibilant.niltype, "niltype")
_val(sibilant.reftype, "reftype")
_val(sibilant.attrtype, "attrtype")
_val(sibilant.undefined, "undefined")
_val(sibilant.compiler.Macro, "macro")

_op((lambda o: o is nil), "nil?")
_op((lambda o: isinstance(o, symbol)), "symbol?")
_op((lambda o: isinstance(o, constype)), "list?")
_op((lambda o: callable(o)), "function?")
_op((lambda o: isinstance(o, macro)), "macro?")

_op(print)
_op(format)
_op(isinstance)
_op(callable)
_op(getattr)
_op(setattr)


__all__ = tuple(__all__)


#
# The end.
