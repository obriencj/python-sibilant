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
from itertools import islice


__all__ = ( "SibilantException",
            "cons", "niltype",
            "main", "cli", "cli_option_parser",
            "car", "cdr",
            "cadr", "caddr", "cadddr", "caddddr",
            "first", "second", "third", "fourth", "fifth", )


class SibilantException(Exception):
    """
    Base class for all Exceptions raised by Sibilant
    """
    pass


class ContinuationCall(BaseException):
    pass


class TrampolineCall(BaseException):
    pass


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


    def __car__(self):
        return self._car


    def __cdr__(self):
        return self._cdr


    def __setcar__(self, value):
        self._car = value


    def __setcdr__(self, value):
        self._cdr = value


    def __getitem__(self, index):
        if index == 0:
            return self.__car__()
        elif index == 1:
            return self.__cdr__()
        else:
            raise IndexError()


    def __setitem__(self, index, value):
        if index == 0:
            self.__setcar__(value)
        elif index == 1:
            self.__setcdr__(value)
        else:
            raise IndexError()


    def __bool__(self):
        return True


    def __repr__(self):
        l = []

        for val in self.items():
            l.append(" ")
            l.append(repr(val))

        if val is nil:
            # if it's a proper list, then we need to pop off the
            # trailing space and nil
            l.pop()
            l.pop()
        else:
            # otherwise, we need to inject a dot to indicate just how
            # improper this list is
            l.insert(-1, ". ")

        l[0] = "("
        l.append(")")

        return "".join(l)


    def items(self):
        """
        iterator that includes a trailing nil for proper lists
        """

        current = self
        while isinstance(current, cons) and (current is not nil):
            yield current.__car__()
            current = current.__cdr__()
        yield current


    def __iter__(self):
        """
        iterator that omits a trailing nil
        """

        current = self
        while isinstance(current, cons) and (current is not nil):
            yield current.__car__()
            current = current.__cdr__()
        if current is not nil:
            yield current


    def is_proper(self):
        return last(self.items()) is nil


class niltype(cons):
    """
    The canonical empty cons cell, nil.
    """

    __slots__ = tuple()

    __nil = None


    def __new__(t):
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


    def __bool__(self):
        return False


    def __repr__(self):
        return "()"


    def __car__(self):
        raise TypeError()


    def __cdr__(self):
        raise TypeError()


    def __setcar__(self, value):
        raise TypeError()


    def __setcdr__(self, value):
        raise TypeError()


    def is_proper(self):
        # according to the Scheme wiki, '() is a proper list
        return True


# This is intended as a singleton
nil = niltype()


car = lambda c: c.__car__()
cdr = lambda c: c.__cdr__()

cadr = lambda c: c.__cdr__().__car__()
caddr = lambda c: c.__cdr__().__cdr__().__car__()
cadddr = lambda c: c.__cdr__().__cdr__().__cdr__().__car__()
caddddr = lambda c: c.__cdr__().__cdr__().__cdr__().__cdr__().__car__()

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

    __slots__ = tuple()

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
        return super(symbol).__hash__()


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
