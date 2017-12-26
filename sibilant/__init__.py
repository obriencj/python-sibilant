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
from itertools import islice
from weakref import WeakValueDictionary

import operator


__all__ = (
    "SibilantException", "NotYetImplemented",
    "symbol", "is_symbol",
    "keyword", "is_keyword",

    "cons", "car", "cdr", "nil",
    "setcar", "setcdr",
    "is_nil", "is_pair", "is_proper", "is_recursive",

    "build_proper", "unpack",
    "copy_pairs", "join_pairs", "build_unpack_pair",

    "reapply", "repeat",
)


class TypePredicate(partial):
    def __new__(cls, name, typeobj):
        obj = partial.__new__(cls, typeobj.__instancecheck__)
        obj.__name__ = name or (typeobj.__name__ + "?")
        return obj

    def __repr__(self):
        return "<builtin type predicate %s>" % self.__name__


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


class InternedAtom():

    __slots__ = ("_name", "__weakref__", )

    _intern = WeakValueDictionary()

    _reprf = "<atom %r>"


    def __new__(cls, name):
        name = str(name)

        s = cls._intern.get(name)
        if s is None:
            s = super().__new__(cls)
            s._name = name
            cls._intern[name] = s
        return s


    def __str__(self):
        return self._name


    def __repr__(self):
        return self._reprf % self._name


    def split(self, sep=None, maxsplit=-1):
        cls = type(self)
        return [cls(s) for s in self._name.split(sep, maxsplit)]


    def rsplit(self, sep=None, maxsplit=-1):
        cls = type(self)
        return [cls(s) for s in self._name.rsplit(sep, maxsplit)]


class Symbol(InternedAtom):
    """
    symbol type.

    Symbol instances are automatically interned. Symbols are equal
    only to themselves. Symbols hash the same as their str
    representation.
    """

    _intern = WeakValueDictionary()

    _reprf = "<symbol %r>"


class Keyword(InternedAtom):
    """
    keyword type.

    keywords are symbols used only as delimeters or markup. evaluation
    of a keyword as an expression only ever results in the keyword.
    """

    _intern = WeakValueDictionary()

    _reprf = "<keyword %r>"


    def __new__(cls, name):
        return super().__new__(cls, str(name).strip(":"))


try:
    from ctypes import symbol, keyword

except ImportError:
    symbol = Symbol
    keyword = Keyword

else:
    del InternedAtom
    del Symbol
    del Keyword


is_symbol = TypePredicate("symbol?", symbol)
is_keyword = TypePredicate("keyword?", keyword)


class Pair(object):
    """
    cons cell type.

    Singly-linked list implementation. Use `car(l)` to get the head or
    value of `l`. Use `cdr(l)` to get the next cons cell in the
    list. The singleton `nil` represents an empty cons cell, or the
    end of a list.
    """

    __slots__ = ("_car", "_cdr", "_src_pos", )


    def __init__(self, head, tail):
        self._car = head
        self._cdr = tail
        self._src_pos = None


    def __getitem__(self, index):
        if index == 0:
            return self._car
        elif index == 1:
            return self._cdr
        else:
            raise IndexError()


    def __setitem__(self, index, value):
        if index == 0:
            self._car = value
        elif index == 1:
            self._cdr = value
        else:
            raise IndexError()


    def __len__(self):
        return 2


    def __iter__(self):
        yield self._car
        yield self._cdr


    def __bool__(self):
        return True


    def __eq__(self, other):
        _Pair = type(self)

        if self is other:
            return True
        elif type(other) is not _Pair:
            return False

        left = self
        right = other

        seen = dict()

        while left is not right:
            if (left is nil) or (right is nil):
                return False
            elif (type(left) is not _Pair):
                return left == right

            left_id = id(left)
            right_id = id(right)

            a, left = left
            b, right = right

            if seen.get(left_id, None) == right_id:
                return True
            elif seen.get(right_id, None) == right_id:
                return True
            elif a != b:
                return False
            else:
                seen[left_id] = right_id
                seen[right_id] = left_id
        else:
            return True


    def __ne__(self, other):
        return not self.__eq__(other)


    def __str__(self):
        _Pair = type(self)
        col = list()
        found = set()

        rest = self
        while rest is not nil:
            if type(rest) is _Pair:
                if id(rest) in found:
                    # recursive
                    col.append(" ...")
                    break
                else:
                    # normal so far
                    found.add(id(rest))
                    val, rest = rest
                    col.append(" ")
                    if isinstance(val, str):
                        val = '"%s"' % val.replace('"', r'\"')
                    else:
                        val = str(val)
                    col.append(val)
            else:
                # end of improper list
                col.append(" . ")
                col.append(str(rest))
                break

        col[0] = "("
        col.append(")")

        return "".join(col)


    def __repr__(self):
        _Pair = type(self)
        col = ["cons("]
        found = {}
        index = 0

        rest = self
        while type(rest) is _Pair:
            rid = id(rest)
            found_at = found.get(rid, None)
            if found_at is None:
                # we insert two elements for every non-recursive
                # entry
                index += 2
                found[rid] = index
                val, rest = rest
                col.append(repr(val))
                col.append(", ")
            else:
                col.append("recursive=True")
                if rest is not self:
                    col.insert(found_at - 1, "cons(")
                    col.append(")")
                break
        else:
            col.append(repr(rest))

        col.append(")")
        return "".join(col)


    def count(self):
        """
        number of items in this list, omitting trailing nil for proper
        lists. Stops counting if/when recursive cells are detected.
        """

        i = 0
        for i, _v in enumerate(self.follow(), 1):
            pass
        return i


    def items(self):
        """
        iterator that includes a trailing nil for proper lists. Recursive
        cons lists result in infinite items
        """

        _Pair = type(self)

        current = self
        while isinstance(current, _Pair) and (current is not nil):
            yield current._car
            current = current._cdr
        yield current


    def take(self, count):
        """
        iterator showing the first `count` results from the `items()`
        method
        """

        return islice(self.items(), 0, count)


    def unpack(self):
        """
        iterator that omits a trailing nil.

        if the list formed by this cons cell is recursive, unpack
        stops emiting items once recursion is detected.
        """

        _Pair = type(self)

        for item in self.follow():
            if type(item) is _Pair:
                yield item._car
            else:
                yield item


    def follow(self):
        """
        iterator that begins with self, and then emits cdr(self),
        cddr(self), etc, until reaching a trailing nil or recursive
        link. If the pair is improper, the last result will not be a
        pair.
        """

        _Pair = type(self)

        found = set()
        current = self

        while (type(current) is _Pair):
            if id(current) in found:
                break

            found.add(id(current))
            yield current
            current = current._cdr

        else:
            if current is not nil:
                yield current


    def is_recursive(self):
        """
        false if this is an improper list, or terminates with a nil
        """

        _Pair = type(self)

        found = set()
        current = self
        while type(current) is _Pair:
            if id(current) in found:
                return True
            else:
                found.add(id(current))
                current = cdr(current)

        return False


    def is_proper(self):
        """
        proper lists have a trailing nil, or are recursive.

        note that because a Pair is mutable, this value cannot be
        safely cached, and must be rechecked every time.
        """

        _Pair = type(self)

        found = set()
        current = self
        while type(current) is _Pair:
            if id(current) in found:
                return True
            else:
                found.add(id(current))
                current = cdr(current)

        return current is nil


    def clear_position(self, follow=False):
        self._src_pos = None

        if not follow:
            return

        _Pair = type(self)
        clear_pos = _Pair.clear_position

        for pair in self.follow():
            if type(pair) is not _Pair:
                break

            pair._src_pos = None

            value = pair._car
            if type(value) is _Pair:
                clear_pos(value, True)


    def set_position(self, position, follow=False):
        self._src_pos = position

        if not follow:
            return

        _Pair = type(self)
        set_pos = _Pair.set_position

        for pair in self.follow():
            if type(pair) is not _Pair:
                break

            pair._src_pos = position

            value = pair._car
            if type(value) is _Pair:
                set_pos(value, position, True)


    def fill_position(self, position, follow=True):
        if self._src_pos is None:
            self._src_pos = position

        if not follow:
            return

        _Pair = type(self)
        fill_pos = _Pair.fill_position

        for pair in self.follow():
            if type(pair) is not _Pair:
                break

            if pair._src_pos is None:
                pair._src_pos = position
            else:
                position = pair._src_pos

            value = pair._car
            if type(value) is _Pair:
                fill_pos(value, position, True)


    def get_position(self):
        return self._src_pos


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


class Nil(Pair):
    """
    The canonical empty cons cell, nil.
    """

    __slots__ = ()

    __nil = None


    def __new__(cls):
        # make nil a singleton
        nil = cls.__nil
        if nil is None:
            nil = super().__new__(cls)
            cls.__nil = nil
        return nil


    def __init__(self):
        pass


    def __eq__(self, other):
        return self is other


    def __ne__(self, other):
        return self is not other


    def __iter__(self):
        # a little silly, but this seems to be the best way to get an
        # empty iterator
        if False:
            yield None


    def items(self):
        if False:
            yield None


    def count(self):
        return 0


    def __len__(self):
        return 0


    def __bool__(self):
        return False


    def __str__(self):
        return "nil"


    def __repr__(self):
        return "nil"


    def follow(self):
        if False:
            yield None


    def unpack(self):
        if False:
            yield None


    def is_recursive(self):
        return False


    def is_proper(self):
        # according to the Scheme wiki, '() is a proper list
        return True


    def get_position(self):
        return None


    def clear_position(self):
        pass


    def set_position(self, position, follow=False):
        pass


    def fill_position(self, position, follow=True):
        pass


class BuiltinPredicate(partial):
    def __new__(cls, name, call, *args, **kwds):
        check = partial.__new__(cls, call, *args, **kwds)
        check.__name__ = name
        return check

    def __repr__(self):
        return "<builtin predicate %s>" % self.__name__


# This is intended as a singleton
nil = Nil()


def car(c):
    """
    retrieve the first portion of a cons cell
    """

    if c is nil:
        raise TypeError("cannot get car of nil")
    elif not is_pair(c):
        raise TypeError("expected Pair instance")
    else:
        return c._car


def cdr(c):
    """
    retrieve the tail of a cons cell, or the second value for an
    improper cell
    """

    if c is nil:
        raise TypeError("cannot get cdr of nil")
    elif not is_pair(c):
        raise TypeError("expected Pair instance")
    else:
        return c._cdr


def setcar(c, value):
    if c is nil:
        raise TypeError("cannod set car of nil")
    elif not is_pair(c):
        raise TypeError("expected Pair instance")
    else:
        c._car = value
        return value


def setcdr(c, value):
    if c is nil:
        raise TypeError("cannod set car of nil")
    elif not is_pair(c):
        raise TypeError("expected Pair instance")
    else:
        c._cdr = value
        return value


try:
    from .ctypes import car, cdr, setcar, setcdr  # noqa
    from .ctypes import nil, pair

except ImportError:
    pair = Pair

else:
    del Pair
    del Nil


is_pair = TypePredicate("pair?", pair)
is_nil = BuiltinPredicate("nil?", operator.is_, nil)


def repeatedly(value):
    while True:
        yield value


def reapply(fun, data, count):
    while count > 0:
        data = fun(data)
    return data


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


def copy_pair(pair):
    """
    Produces a shallow copy of a cons pair chain.
    """

    if pair is nil:
        return nil

    head, tail = pair
    if is_pair(tail):
        tail = copy_pair(tail)

    result = cons(head, tail)
    result.set_position(pair.get_position())
    return result


def merge_pairs(pairs):
    """
    Given a sequence of cons pairs, merge them into a single long
    chain. This alters the pairs in order to link them together.
    """

    result = nil
    work = result

    for pair in pairs:
        if result is nil:
            work = result = pair

        elif cdr(work) is nil:
            setcdr(work, pair)

        else:
            setcdr(work, cons(cdr(work), pair))

        for i in pair.follow():
            if is_pair(i) and i is not nil:
                work = i

    return result


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
