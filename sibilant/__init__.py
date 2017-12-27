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
Sibilant, a Scheme for Python

:author: Christopher O'Brien  <obriencj@gmail.com>
:license: LGPL v.3
"""


from functools import partial, reduce

import operator

from .ctypes import symbol, keyword
from .ctypes import pair, nil, car, cdr, setcar, setcdr
from .ctypes import merge_pairs
from .ctypes import reapply


__all__ = (
    "SibilantException", "NotYetImplemented",
    "symbol", "is_symbol",
    "keyword", "is_keyword",

    "cons", "car", "cdr", "nil",
    "setcar", "setcdr",
    "is_nil", "is_pair", "is_proper", "is_recursive",

    "build_proper", "unpack",
    "copy_pair", "merge_pairs", "build_unpack_pair",

    "reapply", "repeat",
)


class SibilantException(Exception):
    """
    Base class for error-driven Exceptions raised by Sibilant
    """
    pass


class NotYetImplemented(SibilantException):
    """
    Raised as a placeholder for features that haven't been implemented
    yet.
    """
    pass


class TypePredicate(partial):
    def __new__(cls, name, typeobj):
        obj = partial.__new__(cls, typeobj.__instancecheck__)
        obj.__name__ = name or (typeobj.__name__ + "?")
        return obj

    def __repr__(self):
        return "<builtin type predicate %s>" % self.__name__


class BuiltinPredicate(partial):
    def __new__(cls, name, call, *args, **kwds):
        check = partial.__new__(cls, call, *args, **kwds)
        check.__name__ = name
        return check

    def __repr__(self):
        return "<builtin predicate %s>" % self.__name__


is_symbol = TypePredicate("symbol?", symbol)
is_keyword = TypePredicate("keyword?", keyword)

is_pair = TypePredicate("pair?", pair)
is_nil = BuiltinPredicate("nil?", operator.is_, nil)


def cons(head, *tail, recursive=False):
    self = pair(head, None)

    if tail:
        if len(tail) == 1:
            tail = tail[0]

        else:
            def cons(tail, head):
                return pair(head, tail)

            if recursive:
                tail = reduce(cons, reversed(tail), self)
            else:
                tail = reduce(cons, reversed(tail))

    else:
        tail = self if recursive else nil

    setcdr(self, tail)
    return self


def is_proper(value):
    return is_pair(value) and value.is_proper()


def is_recursive(value):
    return is_pair(value) and value.is_recursive()


def build_proper(*values):
    """
    Create a proper cons pair from values
    """

    return cons(*values, nil) if values else nil


def unpack(pair):
    try:
        return pair.unpack()
    except AttributeError:
        return iter(pair)


def get_position(value, default=None):
    return (value.get_position() or default) if is_pair(value) else default


def set_position(value, position, follow=False):
    if position and is_pair(value):
        value.set_position(position, follow)


def fill_position(value, position, follow=True):
    if position and is_pair(value):
        value.fill_position(position, follow)


def repeatedly(value):
    while True:
        yield value


cadr = lambda c: car(cdr(c))  # noqa
caddr = lambda c: car(reapply(cdr, c, 2))  # noqa
cadddr = lambda c: car(reapply(cdr, c, 3))  # noqa
caddddr = lambda c: car(repply(cdr, c, 4))  # noqa
cadddddr = lambda c: car(repply(cdr, c, 5))  # noqa
caddddddr = lambda c: car(repply(cdr, c, 6))  # noqa
cadddddddr = lambda c: car(repply(cdr, c, 7))  # noqa
caddddddddr = lambda c: car(repply(cdr, c, 8))  # noqa
cadddddddddr = lambda c: car(repply(cdr, c, 9))  # noqa
caddddddddddr = lambda c: car(repply(cdr, c, 10))  # noqa

first = car
second = cadr
third = caddr
fourth = cadddr
fifth = caddddr
sixth = cadddddr
seventh = caddddddr
eighth = cadddddddr
ninth = caddddddddr
tenth = cadddddddddr


def last(seq, empty=None):
    """
    returns the last item in an iterable sequence, or undefined if the
    sequence is empty
    """

    if is_pair(seq):
        seq = seq.unpack()

    val = empty
    for val in iter(seq):
        pass
    return val


# === quasiquote magic ===


def copy_pair(p):
    """
    Produces a shallow copy of a cons pair chain.
    """

    # this also allows the copy.copy API to work
    return p.__copy__()


def build_unpack_pair(*seqs):
    """
    Given a series of sequences, create a cons pair chain with the
    contents of each sequence chained together, in order.
    """

    pairs = []

    for seq in seqs:
        if not seq:
            continue
        elif is_pair(seq):
            seq = copy_pair(seq)
        else:
            seq = cons(*seq, nil)
        pairs.append(seq)

    return merge_pairs(pairs)


#
# The end.
