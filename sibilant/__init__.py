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


__all__ = ( "SibilantException",
            "cons", "niltype",
            "main", "cli", "cli_option_parser", )


class SibilantException(Exception):
    """
    Base class for all Exceptions raised by Sibilant
    """
    pass


class cons(object):
    """
    a cons cell
    """

    __slots__ = ( "car", "cdr" )

    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr

    def __repr__(self):
        l = list(iter(self))
        if l[-1] is nil:
            return ("(" + " ".join(map(repr, l[:-1])) + ")")
        else:
            return ("(" + " ".join(map(repr, l[:-1])) +
                    " . " + repr(l[-1]) + ")")

    def __iter__(self):
        current = self
        while type(current) is cons:
            yield current.car
            current = current.cdr
        yield current


class niltype(cons):
    __slots__ = ()
    _nil = None

    def __new__(t):
        nil = t._nil
        if nil is None:
            nil = super().__new__(t)
            t._nil = nil
        return nil

    def __init__(self):
        pass

    def __iter__(self):
        pass

    def __bool__(self):
        return False

    def __repr__(self):
        return "()"


nil = niltype()


def cli(options, args):
    if not args:
        repl(options)
    else:
        filename, *args = args
        with open(filename, "r") as fd:
            sys.argv = args
            load(fd, "__main__")()


def cli_option_parser():
    from optparse import OptionParser

    parser = OptionParser()

    # todo: add CLI options

    return parser


def main(args):
    parser = cli_option_parser()
    options, args = parser.parse_args(args)

    # todo: arg checking, emit problems using `parser.error`

    try:
        cli(options, args)

    except KeyboardInterrupt as keyi:
        return -130

    except SibilantException as sibe:
        return -1

    else:
        return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv))


#
# The end.
