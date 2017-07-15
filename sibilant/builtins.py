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
builtin definitions for sibilant. These are all following k-style
conventions.

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


import operator
import sibilant

from functools import reduce


__all__ = ["is_special"]


def is_special(f):
    return callable(getattr(f, "__special__", False))


def _reduce_op(opf, name=None):
    # in the future, this can become a special. Ops invoked with two
    # arguments can result in the normal call. Ops invoked with more
    # than two arguments can be wrapped in a reduce call.

    def fun(*a):
        return reduce(opf, a)

    name = name if name else opf.__name__

    fun.__name__ = name
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

_op(print, "print")

_op(sibilant.cons)
_op(sibilant.car)
_op(sibilant.cdr)
_op(sibilant.ref)
_op(sibilant.attr)
_op(sibilant.deref)
_op(sibilant.setref)

_val(sibilant.symbol, "symbol")
_val(sibilant.nil, "nil")
_val(sibilant.constype, "constype")
_val(sibilant.niltype, "niltype")
_val(sibilant.reftype, "reftype")
_val(sibilant.attrtype, "attrtype")
_val(sibilant.undefined, "undefined")


__all__ = tuple(__all__)


#
# The end.
