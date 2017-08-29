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


from functools import reduce
from itertools import islice
from weakref import WeakValueDictionary


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


symbol = Symbol


def is_symbol(value):
    return type(value) is Symbol


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


keyword = Keyword


def is_keyword(value):
    return type(value) is Keyword


class Pair(object):
    """
    cons cell type.

    Singly-linked list implementation. Use `car(l)` to get the head or
    value of `l`. Use `cdr(l)` to get the next cons cell in the
    list. The singleton `nil` represents an empty cons cell, or the
    end of a list.
    """

    __slots__ = ("_car", "_cdr", "_src_pos", )


    def __init__(self, head, *tail, recursive=False):
        ltype = type(self)

        if tail:
            def cons(t, h):
                return ltype(h, t)

            if recursive:
                tail = reduce(cons, reversed(tail), self)
            else:
                tail = reduce(cons, reversed(tail))

        elif recursive:
            tail = self

        else:
            raise TypeError("Pair requires at least one tail, or must"
                            " be recursive")

        self._car = head
        self._cdr = tail


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

        if type(other) is not _Pair:
            return False

        left = self
        right = other

        lf = dict()
        rf = dict()

        index = 1

        while left is not right:
            if (left is nil) or (right is nil):
                return False
            elif (type(left) is not _Pair):
                return left == right

            a, left = left
            b, right = right
            if a != b:
                return False
            lin = lf.get(id(left), 0)
            rin = rf.get(id(right), 0)
            if lin != rin:
                return False
            elif lin:
                return True
            else:
                lf[id(left)] = index
                rf[id(right)] = index
                index += 1
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
        iterator that emits cdr(self), until reaching a trailing
        nil or recursive link. If the pair is improper, the last
        result will not be a pair.
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
        try:
            del self._src_pos
        except:
            pass

        if not follow:
            return

        _Pair = type(self)
        clear_pos = _Pair.clear_position

        for pair in self.follow():
            if type(pair) is not _Pair:
                break

            try:
                del pair._src_pos
            except:
                pass

            value = pair._cdr
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

            value = pair._cdr
            if type(value) is _Pair:
                set_pos(value, position, True)


    def fill_position(self, position, follow=True):
        try:
            self._src_pos
        except:
            self._src_pos = position

        if not follow:
            return

        _Pair = type(self)
        fill_pos = _Pair.fill_position

        for pair in self.follow():
            if type(pair) is not _Pair:
                break

            try:
                position = pair._src_pos
            except:
                pair._src_pos = position

            value = pair._cdr
            if type(value) is _Pair:
                fill_pos(value, position, True)


    def get_position(self):
        try:
            return self._src_pos
        except:
            return None


cons = Pair


def is_pair(value):
    return isinstance(value, Pair)


def is_proper(value):
    return isinstance(value, Pair) and value.is_proper()


def is_recursive(value):
    return isinstance(value, Pair) and value.is_recursive()


def build_proper(*values):
    """
    Create a proper cons pair from values
    """

    return cons(*values, nil) if values else nil


def unpack(pair):
    try:
        yield from pair.unpack()
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


def _setup_nil():
    # This is intended as a singleton
    nil = Nil()


    def is_nil(value):
        return value is nil

    return nil, is_nil


nil, is_nil = _setup_nil()


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
    if pair is nil:
        return nil

    head, tail = pair
    if is_pair(tail):
        tail = copy_pair(tail)

    result = cons(head, tail)
    result.set_position(pair.get_position())
    return result


def join_pairs(pairs):
    result = nil
    work = result

    for pair in pairs:
        if result is nil:
            result = pair
            work = result

        elif work._cdr is nil:
            work._cdr = pair

        else:
            work._cdr = cons(work._cdr, pair)

        for i in pair.follow():
            if is_pair(i):
                work = i

    return result


def build_unpack_pair(*seqs):
    pairs = []

    for seq in seqs:
        if is_pair(seq):
            seq = copy_pair(seq)
        elif seq:
            seq = cons(*seq, nil)
        else:
            continue
        pairs.append(seq)

    return join_pairs(pairs)


#
# The end.
