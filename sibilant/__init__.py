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

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


import sys


__all__ = ( "SibilantException", "NotYetImplemented",
            "main", "cli", "cli_option_parser",
            "symbol",
            "cons", "car", "cdr",
            "nil", "niltype",
            "ref", "attr", "deref", "setref",
            "undefined", "undefinedtype", )


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


class undefinedtype(object):
    """
    an undefined value. singleton indicating a value has not been
    assigned to a ref yet.
    """

    __instance = None

    def __new__(k):
        inst = k.__instance
        if inst is None:
            inst = super().__new__(k)
            k.__instance = inst
        return inst

    def __repr__(self):
        return "#<unspecified>"

    def __str__(self):
        return "#<unspecified>"


# undefined singleton
undefined = undefinedtype()


class ref(object):
    """
    mutable reference
    """

    __slots__ = ("sym", "_value")


    def __init__(self, sym, value=undefinedtype()):
        if type(sym) is not symbol:
            raise TypeError("sym must be a symbol")

        self.sym = sym
        self._value = value


    def _get_value(self, k):
        return k(self._value)


    def _set_value(self, k, value):
        self._value = value
        return k(value)


    def __repr__(self):
        return "%s(%r)" % (type(self).__name__, self.sym)


class attr(ref):
    """
    computed attribute references
    """

    __slots__ = ("sym", "_get_value", "_set_value")


    def __init__(self, sym, getter, setter=None):
        self.sym = sym
        self._get_value = getter
        self._set_value = setter or self._ro_value


    def _ro_value(self, k, value):
        raise AttributeError("%s is read-only" % str(self.sym))


def _return_v(v):
    """
    simple continuation that uses python's in-built return to pass the
    value back to the original caller.
    """
    return v


def deref(r):
    return deref_k(_return_v, r)


def deref_k(k, r):
    if isinstance(r, ref):
        return r._get_value(k)
    else:
        raise TypeError("expected ref instance")


def setref(r, value):
    return setref_k(_return_v, r, value)


def setref_k(k, r, value):
    if isinstance(r, ref):
        return r._set_value(k, value)
    else:
        raise TypeError("expected ref instance")


class cons(object):
    """
    cons cell type.

    Singly-linked list implementation. Use `car(l)` to get the head or
    value of `l`. Use `cdr(l)` to get the next cons cell in the
    list. The singleton `nil` represents an empty cons cell, or the
    end of a list.
    """

    __slots__ = ( "_car", "_cdr" )


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


    def __str__(self):
        col = list()
        found = set()

        rest = self
        while rest is not nil:
            if type(rest) is cons:
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
        col = list()
        found = set()

        rest = self
        while type(rest) is cons:
            if id(rest) in found:
                # recursive
                col.append("...")
                break
            else:
                # normal so far
                found.add(id(rest))
                val, rest = rest
                col.append("cons(")
                col.append(repr(val))
                col.append(", ")
        else:
            col.append(repr(rest))

        col.append(")"*len(found))
        return "".join(col)


    def count(self):
        """
        recursive count of items in this list, omitting trailing nil for
        proper lists. Stops counting if/when recursive cells are
        detected.
        """

        i = -1
        for i, v in enumerate(self.unpack()): pass
        return i + 1


    def items(self):
        """
        iterator that includes a trailing nil for proper lists. Recursive
        cons lists result in infinite items
        """

        current = self
        while isinstance(current, cons) and (current is not nil):
            yield current._car
            current = current._cdr
        yield current


    def unpack(self):
        """
        iterator that omits a trailing nil.

        if the list formed by this cons is recursive, unpack stops
        emiting items once recursion is detected.
        """

        found = set()
        current = self

        while (type(current) is cons) and (id(current) not in found):
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
        while type(current) is cons:
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
        while type(current) is cons:
            if id(current) in found:
                return True
            else:
                found.add(id(current))
                current = cdr(current)

        return current is nil


class niltype(cons):
    """
    The canonical empty cons cell, nil.
    """

    __slots__ = ()

    __nil = None


    def __new__(t):
        # make nil a singleton
        nil = t.__nil
        if nil is None:
            nil = super().__new__(t)
            t.__nil = nil
        return nil


    def __init__(self):
        pass


    def __iter__(self):
        # a little silly, but this seems to be the best way to get an
        # empty iterator
        if False:
            yield None


    def items(self):
        if False:
            yield None


    def __len__(self):
        return 0


    def __bool__(self):
        return False


    def __str__(self):
        return "()"


    def __repr__(self):
        return "niltype()"


    def is_recursive(self):
        return False


    def is_proper(self):
        # according to the Scheme wiki, '() is a proper list
        return True


# This is intended as a singleton
nil = niltype()


def car(c):
    """
    retrieve the first portion of a cons cell
    """

    if c is nil:
        raise TypeError("cannot get car of nil")
    elif type(c) is not cons:
        raise TypeError("expected cons instance")
    else:
        return c._car


def cdr(c):
    """
    retrieve the tail of a cons cell, or the second value for an
    improper cell
    """

    if c is nil:
        raise TypeError("cannot get cdr of nil")
    elif type(c) is not cons:
        raise TypeError("expected cons instance")
    else:
        return c._cdr


def setcar(c, value):
    if c is nil:
        raise TypeError("cannod set car of nil")
    elif type(c) is not cons:
        raise TypeError("expected cons instance")
    else:
        c._car = value
        return value


def setcdr(c, value):
    if c is nil:
        raise TypeError("cannod set car of nil")
    elif type(c) is not cons:
        raise TypeError("expected cons instance")
    else:
        c._cdr = value
        return value


cadr = lambda c: car(cdr(c))
caddr = lambda c: car(cdr(cdr(c)))
cadddr = lambda c: car(cdr(cdr(cdr(c))))
caddddr = lambda c: car(cdr(cdr(cdr(cdr(c)))))

first = car
second = cadr
third = caddr
fourth = cadddr
fifth = caddddr


def last(seq):
    """
    returns the last item in an iterable sequence.
    """

    for val in iter(seq): pass
    return val


class symbol(str):
    """
    symbol type.

    Symbol instances are automatically interned. Symbols are equal
    only to themselves. Symbols hash the same as their str
    representation.
    """

    __slots__ = ()

    __intern = {}


    def __new__(t, name):
        name = str(name)
        s = t.__intern.get(name)
        if s is None:
            s = super().__new__(t, name)
            t.__intern[name] = s
        return s


    def __repr__(self):
        return "symbol({})".format(repr(str(self)))


    def __eq__(self, other):
        return self is other


    def __ne__(self, other):
        return self is not other


    def __hash__(self):
        return super(symbol, self).__hash__()


def _k_wrap(fun):
    def fun_k(k, *args):
        return k(fun(*args))
    update_wrapper(fun_k, fun)
    return fun_k


def cli(options, args):
    """
    Run as from the command line, with the given options argument and
    additional positional args
    """

    if not args:
        repl(options)
    else:
        filename, *args = args
        with open(filename, "r") as fd:
            sys.argv = args
            load(fd, "__main__")()


def cli_option_parser():
    """
    Create an `OptionParser` instance with the options requested by
    the `cli` function
    """

    from optparse import OptionParser

    parser = OptionParser()

    # todo: add CLI options

    return parser


def main(args):
    """
    Invoked when module is run as __main__
    """

    parser = cli_option_parser()
    options, args = parser.parse_args(args)

    # todo: arg checking, emit problems using `parser.error`

    try:
        cli(options, args)

    except KeyboardInterrupt as keyi:
        return -130

    else:
        return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv))


#
# The end.
