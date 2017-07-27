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
Abstract Syntax Tree for Sibilant

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


from abc import ABCMeta, abstractmethod
from fractions import Fraction as fraction
from functools import partial
from io import StringIO

from . import cons, nil, symbol
from . import SibilantException
from .parse import Event, parse


__all__ = (
    "SyntaxError",
    "Node", "List", "Atom", "Symbol",
    "Literal", "Number", "Integer", "Decimal",
    "Fraction", "Complex", "Nil", "String",
    "Marked", "Quote", "Quasi", "Unquote", "Splice",
    "compose", "compose_from_str",
    "compose_all_from_stream", "compose_all_from_str",
)


class SyntaxError(SibilantException):
    """
    An error while parsing sibilant code
    """
    pass


class Node(object):
    """
    Base class for all AST node types
    """

    __metaclass__ = ABCMeta


    def __init__(self, position=(1, 0)):
        self.position = position


    @abstractmethod
    def simplify(self, positions):
        pass


    def __ne__(self, other):
        return not (self == other)


    def __repr__(self):
        return "%s(position=%r)" % (type(self).__name__, self.position)


    def __str__(self):
        return repr(self)


class List(Node):
    """
    A collection of sub-expressions
    """

    def __init__(self, position, *members):
        self.position = position
        self.proper = True
        self.members = list(members)


    def simplify(self, positions):
        c = None

        for member in self.members[::-1]:
            s = member.simplify(positions)
            if c is None:
                c = cons(s, nil) if self.proper else s
            else:
                c = cons(s, c)

            positions[id(c)] = member.position

        if c is None:
            c = nil

        return c


    def __repr__(self):
        data = (type(self).__name__,
                self.position,
                self.proper,
                ",".join(map(repr, self.members)))
        return "%s(position=%r,proper=%r,members=[%s])" % data


    def __eq__(self, other):
        return ((type(self) is type(other)) and
                (self.position == other.position) and
                (self.proper == other.proper) and
                (self.members == other.members))


class Atom(Node):
    """
    Parent class for single-token expressions
    """

    def __init__(self, position, token):
        self.position = position
        self.token = token


    def __eq__(self, other):
        return ((type(self) is type(other)) and
                (self.position == other.position) and
                (self.token == other.token))


    def __repr__(self):
        data = (type(self).__name__,
                self.position,
                self.token)
        return "%s(position=%r,%r)" % data


class Symbol(Atom):

    def __new__(cls, position, token):
        if token in ("#t", "True"):
            cls = True_
        elif token in ("#f", "False"):
            cls = False_
        elif token == "None":
            cls = None_
        elif token == "...":
            cls = Ellipsis_

        return super().__new__(cls)


    def simplify(self, positions):
        return symbol(self.token)


class Literal(Atom):
    pass


class Number(Literal):

    def __new__(cls, position, token):
        if cls is Number:
            cls = Integer

            if token[-1] in "ij":
                cls = Complex
            elif "/" in token:
                cls = Fraction
            elif "." in token:
                cls = Decimal

        return super().__new__(cls)


class Integer(Number):

    def simplify(self, positions):
        return int(self.token, 0)


class Decimal(Number):

    def simplify(self, positions):
        return float(self.token)


class Fraction(Number):

    def simplify(self, positions):
        return fraction(self.token)


class Complex(Number):

    def simplify(self, positions):
        t = self.token
        if t[-1] == "i":
            return complex(self.token[:-1] + "j")
        else:
            return complex(self.token)


class LiteralSymbol(Literal, Symbol):
    pass


class True_(LiteralSymbol):

    def simplify(self, positions):
        return True


class False_(LiteralSymbol):

    def simplify(self, positions):
        return False


class None_(LiteralSymbol):

    def simplify(self, positions):
        return None


class Ellipsis_(LiteralSymbol):

    def simplify(self, positions):
        return ...


class Nil(LiteralSymbol):

    def simplify(self, positions):
        return nil


class String(Literal):

    def simplify(self, positions):
        return self.token


class Marked(Node):

    def __init__(self, position, expr=None):
        self.position = position
        self.expression = expr


    def __eq__(self, other):
        return ((type(self) is type(other)) and
                (self.position == other.position) and
                (self.expression == other.expression))


    def __repr__(self):
        data = (type(self).__name__,
                self.position,
                self.expression)
        return "%s(position=%r,expr=%r" % data


    def simplify(self, positions):
        s = cons(self._q, self.expression.simplify(positions))
        positions[id(s)] = self.position
        return s


class Quote(Marked):

    _q = symbol("quote")


class Quasi(Marked):

    _q = symbol("quasiquote")


class Unquote(Marked):

    _q = symbol("unquote")


class Splice(Marked):

    _q = symbol("splice")


klass_events = {
    Event.OPEN: List,
    Event.SYMBOL: Symbol,
    Event.STRING: String,
    Event.NUMBER: Number,
    Event.QUOTE: Quote,
    Event.QUASI: Quasi,
    Event.UNQUOTE: Unquote,
    Event.SPLICE: Splice,
}


def create_node(position, event, *args):
    klass = klass_events.get(event)
    return klass(position, *args) if klass else None


def compose(parser_gen):
    """
    Composes a single element or statement from the event stream
    `parser_gen`
    """

    stack = list()
    node = None

    for event, position, *data in parser_gen:
        node = create_node(position, event, *data)

        if event == Event.NEWLINE:
            # let the parser count lines for us
            continue

        elif event == Event.COMMENT:
            # don't bother representing comments in the AST
            continue

        elif event == Event.OPEN:
            stack.append(node)
            continue

        elif event == Event.DOT:
            if not stack:
                raise SyntaxError(". without list", position)
            else:
                stack[-1].proper = False
                continue

        elif event == Event.CLOSE:
            node = stack.pop()

        elif event in (Event.QUOTE, Event.QUASI,
                       Event.UNQUOTE, Event.SPLICE):

            marked = compose(parser_gen)
            if marked is None:
                raise SyntaxError("unterminated mark", position)
            node.expression = marked

        elif event in (Event.SYMBOL, Event.NUMBER,
                       Event.STRING):
            pass

        # finished lists, literals should reach here
        assert(node is not None)
        if stack:
            stack[-1].members.append(node)
        else:
            break

    if stack:
        raise SyntaxError("unterminated list", position)

    return node


def compose_from_stream(stream):
    """
    compose an AST from an input stream
    """

    pgen = parse(stream)
    return compose(pgen)


def compose_from_str(src_str):
    """
    compose an AST from src_str
    """

    pgen = parse(StringIO(src_str))
    return compose(pgen)


def compose_all_from_stream(stream):
    """
    compose all AST from input stream
    """

    pgen = parse(stream)
    return iter(partial(compose, pgen), None)


def compose_all_from_str(src_str):
    """
    compose all AST from src_str
    """

    pgen = parse(StringIO(src_str))
    return iter(partial(compose, pgen), None)


#
# The end.
