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


from functools import partial
from itertools import chain, islice, repeat

import operator

from ._types import symbol, keyword, gensym
from ._types import pair, nil, cons, car, cdr, setcar, setcdr
from ._types import build_unpack_pair
from ._types import reapply
from ._types import build_tuple, build_list, build_set, build_dict
from ._types import values
from ._types import getderef, setderef, clearderef


__all__ = (
    "SibilantException", "NotYetImplemented",
    "symbol", "is_symbol",
    "keyword", "is_keyword",

    "gensym",
    "lazygensym", "is_lazygensym",

    "pair", "cons", "nil",
    "car", "cdr", "setcar", "setcdr",
    "is_nil", "is_pair", "is_proper", "is_recursive",

    "build_proper", "unpack",
    "build_unpack_pair",

    "reapply", "repeatedly", "last", "take",

    "build_tuple", "build_list", "build_set", "build_dict",

    "values",

    "getderef", "setderef", "clearderef",
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


class SibilantSyntaxError(SyntaxError):
    """
    An error in sibilant syntax, either during read or compile time.
    """

    def __init__(self, message, location=None, filename=None, text=None):
        if filename:
            if not location:
                location = (1, 0)
            super().__init__(message, (filename, *location, text))
            self.print_file_and_line = True
        else:
            super().__init__(message)


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


def is_proper(value):
    return is_pair(value) and value.is_proper()


def is_recursive(value):
    return is_pair(value) and value.is_recursive()


def build_proper(*values):
    """
    Create a proper cons pair from values
    """

    return cons(*values, nil) if values else nil


def unpack(value):
    return value.unpack() if is_pair(value) else iter(value)


def get_position(value, default=None):
    return (value.get_position() or default) if is_pair(value) else default


def set_position(value, position, follow=False):
    if position and is_pair(value):
        value.set_position(position, follow)


def fill_position(value, position, follow=True):
    if position and is_pair(value):
        value.fill_position(position, follow)


def apply(fun, args=(), kwargs={}):
    # todo: move to _types
    if is_pair(args):
        args = args.unpack()
    return fun(*args, **kwargs)


def repeatedly(work, *args, **kwds):
    # todo: move to _types
    if args or kwds:
        invoke = values(*args, **kwds)
        while True:
            yield invoke(work)
    else:
        while True:
            yield work()


cadr = lambda c: car(cdr(c))  # noqa
caddr = lambda c: car(reapply(cdr, c, 2))  # noqa
cadddr = lambda c: car(reapply(cdr, c, 3))  # noqa
caddddr = lambda c: car(reapply(cdr, c, 4))  # noqa
cadddddr = lambda c: car(reapply(cdr, c, 5))  # noqa
caddddddr = lambda c: car(reapply(cdr, c, 6))  # noqa
cadddddddr = lambda c: car(reapply(cdr, c, 7))  # noqa
caddddddddr = lambda c: car(reapply(cdr, c, 8))  # noqa
cadddddddddr = lambda c: car(reapply(cdr, c, 9))  # noqa
caddddddddddr = lambda c: car(reapply(cdr, c, 10))  # noqa

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
    returns the last item in an iterable sequence, or a default empty
    value if the sequence has no items
    """

    if is_pair(seq):
        seq = seq.unpack()

    val = empty
    for val in iter(seq):
        pass
    return val


class sentinel():
    def __init__(self, words):
        self._r = "<{}>".format(words)

    def __repr__(self):
        return self._r


omit_padding = sentinel("omit padding")


def take(seq, count, padding=omit_padding):
    """
    returns a list of up to count items taken from the given sequence.

    if padding is specified, then any sequence too short will be
    padded at its end with the given value
    """

    if padding is omit_padding:
        return list(islice(seq, count))
    else:
        return list(islice(chain(seq, repeat(padding)), count))


class lazygensym(object):


    def __init__(self, name=None, predicate=None):
        self._name = name
        self._predicate = predicate
        self._symbol = None


    def __call__(self):
        sym = self._symbol
        if sym is None:
            sym = gensym(self._name, self._predicate)
            self._symbol = sym
        return sym


    def __str__(self):
        return str(self())


    def __repr__(self):
        sym = self._symbol
        if sym is None:
            return "<lazygensym %s#...>" % self._name
        else:
            return "<lazygensym %s>" % sym


is_lazygensym = TypePredicate("lazygensym?", lazygensym)


#
# The end.
