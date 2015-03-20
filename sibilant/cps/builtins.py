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



class reftype(object):
    """
    mutable reference
    """

    __slots__ = ("sym", "_value")


    def __init__(self, sym, value=undefined):
        if type(sym) is not symbol:
            raise TypeError("sym must be a symbol")

        self.sym = sym
        self._value = value


    def _get_value_k(self, k):
        return k(self._value)


    def _get_value(self):
        return self._value


    def _set_value_k(self, k, value):
        self._value = value
        return k(value)


    def _set_value(self, value):
        self._value = value
        return value


    def __repr__(self):
        return "".join(("ref(", repr(self.sym), ")"))


def ref(sym, value=undefined):
    return reftype(sym, value)


class attrtype(reftype):
    """
    computed attribute references
    """

    __slots__ = ("sym", "_get_value_k", "_set_value_k")


    def __init__(self, sym, getter_k, setter_k=None):
        self.sym = sym
        self._get_value_k = getter_k
        self._set_value_k = setter_k or self._ro_value_k


    def _get_value(self):
        return self._get_value_k(_return_v)


    def _set_value(self, value):
        return self._set_value_k(_return_v, value)


    def _ro_value_k(self, k, value):
        raise AttributeError("%s is read-only" % str(self.sym))


    def __repr__(self):
        return "".join(("attr(", repr(self.sym), ")"))


def attr(sym, getter, setter=None):
    return attrtype(sym, k_wrap(getter), k_wrap(setter) if setter else None)


@k_style
def deref_k(k, r):
    return r._get_value_k(k)


@k_adapt(deref_k)
def deref(r):
    return r._get_value()


@k_style
def setref_k(k, r, value):
    return r._set_value_k(k, value)


@k_adapt(setref_k)
def setref(r, value):
    return r._set_value(value)



@k_style
def last_k(k, seq):
    """
    returns the last item in an iterable sequence, or undefined if the
    sequence is empty
    """

    val = undefined
    for val in iter(seq): pass
    return k(val)


@k_adapt(last_k)
def last(seq):
    """
    returns the last item in an iterable sequence, or undefined if the
    sequence is empty
    """

    val = undefined
    for val in iter(seq): pass
    return val


#
# The end.
