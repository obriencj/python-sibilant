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
from functools import reduce


__all__ = (
    "add", "sub", "mult", "divide", "mod", "pow",
)


def _reduce_op(opf, alias=None):
    fun = lambda k, *a: k(reduce(opf, a))
    fun.__name__ = opf.__name__
    fun.__doc__ = opf.__doc__
    if alias:
        globals()[alias] = fun
    return fun


def _op(opf, alias=None):
    fun = wraps(opf)
    if alias:
        globals()[alias] = fun
    return fun


__add__ = _reduce_op(operator.__add__, "+")
__sub__ = _reduce_op(operator.__sub__, "-")
__mul__ = _reduce_op(operator.__mul__, "*")
__pow__ = _op(operator.__pow__, "**")
__div__ = _op(operator.__div__, "/")
__mod__ = _op(operator.__mod__, "%")
__floordiv__ = _op(operator.__floordiv__, "//")
__bitwise_or__ = _op(operator.__or__, "|")
__bitwise_and__ = _op(operator.__and__, "&")
__bitwise_xor__ = _op(operator.__xor__, "^")
__bitwise_invert__ = _op(operator.__invert__, "~")


#
# The end.
