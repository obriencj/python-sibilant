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


def _number_op_k(opf):
    fun = lambda k, *a: k(reduce(opf, a))
    fun.__name__ = opf.__name__
    fun.__doc__ = opf.__doc__
    return fun


add_k = _number_op_k(numbers.__add__)
sub_k = _number_op_k(numbers.__sub__)
mult_k = _number_op_k(numbers.__mult__)
divide_k = _number_op_k(numbers.__div__)
mod_k = _number_op_k(numbers.__mod__)
pow_k = _number_op_k(numbers.__pow__)


#
# The end.
