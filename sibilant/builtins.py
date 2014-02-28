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
builtin definitions for sibilant

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


from functools import reduce


# we can replace the class supplying number functions here
numbers = float


def number_op_k(opf):
    # creates a k call conforming number operand function
    opf =
    return lambda k, *a: k(reduce(opf, a))


add_k = number_op_k(numbers.__add__)
sub_k = number_op_k(numbers.__sub__)
mult_k = number_op_k(numbers.__mult__)
divide_k = number_op_k(numbers.__div__)
mod_k = number_op_k(numbers.__mod__)
pow_k = number_op_k(numbers.__pow__)


def cons_k(k, item, list):
    return k(cons(item, list))


def car_k(k, list):
    return k(list.car)


def cdr_k(k, list):
    return k(list.cdr)


def list_k(k, *a):
    return k(reduce(lambda x,y: cons(y,x), a[::-1], null))


class symbol(object):
    def __init__(self, s):
        self.sym = s


#
# The end.
