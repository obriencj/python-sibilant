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
    "is_nil", "is_pair", "is_proper",
    "make_proper", "unpack",

    "ref", "attr", "deref", "setref",
    "undefined", "is_undefined",

    "reapply", "repeat",
    "first", "second", "third", "fourth",
    "fifth", "last",
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


class UndefinedType(object):
    """
    an undefined value. singleton indicating a value has not been
    assigned to a ref yet.
    """

    __instance = None


    def __new__(cls):
        inst = cls.__instance
        if inst is None:
            inst = super().__new__(cls)
            cls.__instance = inst
        return inst


    def __repr__(self):
        return "#<unspecified>"


    def __str__(self):
        return "#<unspecified>"


# undefined singleton
undefined = UndefinedType()


def is_undefined(value):
    return value is undefined


class Symbol(object):
    """
    symbol type.

    Symbol instances are automatically interned. Symbols are equal
    only to themselves. Symbols hash the same as their str
    representation.
    """

    __slots__ = ("_name", "__weakref__", )

    __intern = WeakValueDictionary()


    def __new__(cls, name):
        # applying str auto-interns
        name = str(name)

        s = cls.__intern.get(name)
        if s is None:
            s = object.__new__(cls)
            s._name = name
            cls.__intern[name] = s
        return s


    def __eq__(self, other):
        return self is other


    def __ne__(self, other):
        return self is not other


    def __hash__(self):
        return self._name.__hash__()


    def __repr__(self):
        return "".join(("symbol(", repr(self._name), ")"))


    def __str__(self):
        return self._name


    def split(self, sep=None, maxsplit=-1):
        cls = type(self)
        return [cls(s) for s in self._name.split(sep, maxsplit)]


    def rsplit(self, sep=None, maxsplit=-1):
        cls = type(self)
        return [cls(s) for s in self._name.rsplit(sep, maxsplit)]


def symbol(name):
    if isinstance(name, Symbol):
        return name
    else:
        return Symbol(name)


def is_symbol(value):
    return isinstance(value, Symbol)


class Keyword(Symbol):
    """
    keyword type.

    keywords are symbols used only as delimeters or markup. evaluation
    of a keyword as an expression only ever results in the keyword.
    """

    __intern = WeakValueDictionary()


    def __new__(cls, name):
        # applying str auto-interns
        name = str(name).strip(":")

        s = cls.__intern.get(name)
        if s is None:
            s = object.__new__(cls)
            s._name = name
            cls.__intern[name] = s
        return s


    def __repr__(self):
        return "".join(("keyword(", repr(self._name), ")"))


def keyword(name):
    if isinstance(name, Keyword):
        return name
    else:
        return Keyword(name)


def is_keyword(value):
    return isinstance(value, Keyword)


class RefType(object):
    """
    mutable reference
    """

    __slots__ = ("sym", "_value", )


    def __init__(self, sym, value):
        if not is_symbol(sym):
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
    return RefType(sym, value)


class AttrType(RefType):
    """
    computed attribute references
    """

    __slots__ = ("sym", "_get_value", "_set_value", )


    def __init__(self, sym, getter, setter):
        self.sym = sym
        self._get_value = getter
        self._set_value = setter or self._ro_value


    def _ro_value(self, value):
        raise AttributeError("%s is read-only" % str(self.sym))


    def __repr__(self):
        return "".join(("attr(", repr(self.sym), ")"))


def attr(sym, getter, setter=None):
    return AttrType(sym, getter, setter)


def deref(r):
    return r._get_value()


def setref(r, value):
    return r._set_value(value)


class Pair(object):
    """
    cons cell type.

    Singly-linked list implementation. Use `car(l)` to get the head or
    value of `l`. Use `cdr(l)` to get the next cons cell in the
    list. The singleton `nil` represents an empty cons cell, or the
    end of a list.
    """

    __slots__ = ("_car", "_cdr", )


    def __init__(self, car, cdr):
        self._car = car
        self._cdr = cdr


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
        if type(other) is not Pair:
            return False

        left = self
        right = other

        lf = dict()
        rf = dict()

        index = 1

        while left is not right:
            if (left is nil) or (right is nil):
                return False
            elif (type(left) is not Pair):
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
        col = list()
        found = set()

        rest = self
        while rest is not nil:
            if type(rest) is Pair:
                if id(rest) in found:
                    # recursive
                    col.append(" ...")
                    break
                else:
                    # normal so far
                    found.add(id(rest))
                    val, rest = rest
                    col.append(" ")
                    col.append(str(val))
            else:
                # end of improper list
                col.append(" . ")
                col.append(str(rest))
                break

        col[0] = "("
        col.append(")")

        return "".join(col)


    def __repr__(self):
        col = ["cons("]
        found = {}
        index = 0

        rest = self
        while type(rest) is Pair:
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

        i = -1
        for i, _v in enumerate(self.unpack()):
            pass
        return i + 1


    def items(self):
        """
        iterator that includes a trailing nil for proper lists. Recursive
        cons lists result in infinite items
        """

        current = self
        while isinstance(current, Pair) and (current is not nil):
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

        found = set()
        current = self

        while (type(current) is Pair):
            if id(current) in found:
                return

            found.add(id(current))
            yield current._car
            current = current._cdr

        if current is not nil:
            yield current


    def is_recursive(self):
        """
        false if this is an improper list, or terminates with a nil
        """

        found = set()
        current = self
        while type(current) is Pair:
            if id(current) in found:
                return True
            else:
                found.add(id(current))
                current = cdr(current)

        return False


    def is_proper(self):
        """
        proper lists have a trailing nil, or are recursive.
        """

        found = set()
        current = self
        while type(current) is Pair:
            if id(current) in found:
                return True
            else:
                found.add(id(current))
                current = cdr(current)

        return current is nil


def is_pair(value):
    return isinstance(value, Pair)


def is_proper(value):
    return isinstance(value, Pair) and value.is_proper()


def make_proper(*values):
    if values:
        return cons(*values, nil)
    else:
        return nil


def unpack(pair):
    try:
        yield from pair.unpack()
    except AttributeError:
        return iter(pair)


def cons(a, *b, recursive=False, ltype=Pair):
    """
    Construct a singly-linked list from arguments. If final argument
    is not `nil`, the list will be considered improper. If recursive
    is True, the final link in the list will reference the start of
    the list.
    """

    if recursive:
        a = ltype(a, nil)
        setcdr(a, reduce(lambda x, y: ltype(y, x), b[::-1], a))
        return a
    else:
        b, *c = b
        if c:
            b = ltype(b, reduce(lambda x, y: ltype(y, x), c[::-1]))
        return ltype(a, b)


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


    def is_recursive(self):
        return False


    def is_proper(self):
        # according to the Scheme wiki, '() is a proper list
        return True


# This is intended as a singleton
nil = Nil()


def is_nil(value):
    return value is nil


def car(c):
    """
    retrieve the first portion of a cons cell
    """

    if c is nil:
        raise TypeError("cannot get car of nil")
    elif not is_pair(c):
        raise TypeError("expected PairType instance")
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
        raise TypeError("expected PairType instance")
    else:
        return c._cdr


def setcar(c, value):
    if c is nil:
        raise TypeError("cannod set car of nil")
    elif not is_pair(c):
        raise TypeError("expected PairType instance")
    else:
        c._car = value
        return value


def setcdr(c, value):
    if c is nil:
        raise TypeError("cannod set car of nil")
    elif not is_pair(c):
        raise TypeError("expected PairType instance")
    else:
        c._cdr = value
        return value


def repeat(value, count):
    value = (value) * count
    return cons(*value, nil)


def reapply(fun, data, count):
    while count > 0:
        data = fun(data)
    return data


cadr = lambda c: car(cdr(c))  # noqa
caddr = lambda c: car(reapply(cdr, c, 2))  # noqa
cadddr = lambda c: car(reapply(cdr, c, 3))  # noqa
caddddr = lambda c: car(repply(cdr, c, 4))  # noqa

first = car
second = cadr
third = caddr
fourth = cadddr
fifth = caddddr


def last(seq):
    """
    returns the last item in an iterable sequence, or undefined if the
    sequence is empty
    """

    if is_pair(seq):
        seq = seq.items()

    val = undefined
    for val in iter(seq):
        pass
    return val


#
# The end.
