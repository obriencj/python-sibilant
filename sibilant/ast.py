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


from io import StringIO

import sibilant.parse as parse


# TRANSFORM  =  in-place modifications
# TRANSLATE  =  return new instance with modifications


# basics


typep = lambda t: lambda n: isinstance(n, t)


class Node(object):
    """
    Base class for all AST node types
    """

    def __init__(self, position=(1, 0)):
        self.position = position

    def translate(self):
        return self

    def __ne__(self, other):
        return not (self == other)

    def __repr__(self):
        return "%s(position=%r)" % (type(self).__name__, self.position)

    def __str__(self):
        return repr(self)


class Comment(Node):
    """
    A comment
    """

    def __init__(self, position, txt):
        self.position = position
        self.text = txt

    def __eq__(self, other):
        return ((type(self) is type(other)) and
                (self.position == other.position) and
                (self.text == other.text))

    def __repr__(self):
        data = (type(self).__name__,
                self.position,
                self.text)

        return "%s(position=%r,txt=%r)" % data


class Marked(Node):
    """
    Parent class for mark indicators which augment another expression
    """

    def __init__(self, position, expression=None):
        self.position = position
        self.expression = expression


    def __eq__(self, other):
        return ((type(self) is type(other)) and
                (self.position == other.position) and
                (self.expression == other.expression))


    def __repr__(self):
        data = (type(self).__name__,
                self.position,
                self.expression)

        return "%s(position=%r,%r)" % data


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


class List(Node):
    """
    A collection of sub-expressions
    """

    def __init__(self, position, *members):
        self.position = position
        self.proper = True
        self.members = list(members)


    def translate(self):
        if self.members:
            fun = self.members[0]
            param = self.members[1:]

            # if member[0] is a symbol, we expand into a specific
            # special form instance and then translate that
            if is_symbol(fun):
                klass = specials.get(fun.token)
                if klass:
                    #print("list into special", klass)
                    tmp = klass(self.position, *param)
                    tmp.transform()
                    return tmp

            # if member[0] isn't a symbol, or isn't a symbol that is
            # considered special, it's a function application

            membs = [m.translate() for m in self.members]
            return Apply(self.position, fun, *param)

        else:
            return self


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


class Symbol(Atom):
    pass


class Number(Atom):
    pass


class String(Atom):
    pass


# sharps


class Sharp(Marked):

    def translate(self):
        if is_symbol(self.expression):
            ident = self.expression.token
            if ident in "ft":
                return Boolean(self.position, ident)
            elif ident in "bodx":
                # xxx
                return Number(self.position, self.expression)
            elif ident in "ie":
                # xxx
                return Number(self.position, self.expression)
            elif ident == "\\":
                # xxx
                return Character(self.position, self.expression)

        elif is_list(self.expression):
            return Vector(self.position, self.expression.members)

        else:
            pass


class Boolean(Atom):
    pass


class Character(Atom):
    pass


# quotes


class Quote(Marked):
    pass


class Quasi(Marked):
    pass


class Unquote(Marked):
    pass


class Splice(Marked):
    pass


klass_events = {
    parse.E_SYMBOL: Symbol,
    parse.E_NUMBER: Number,
    parse.E_SHARP: Sharp,
    parse.E_STRING: String,
    parse.E_QUOTE: Quote,
    parse.E_QUASI: Quasi,
    parse.E_UNQUOTE: Unquote,
    parse.E_SPLICE: Splice,
    parse.E_OPEN: List,
    parse.E_COMMENT: Comment,
}


def create_node(position, event, *args):
    klass = klass_events.get(event)
    return klass(position, *args) if klass else None


def compose(parser_gen):
    """
    Composes a single element or statement from the event stream
    `parser_gen`
    """

    ret = None
    stack = list()

    for event, position, *data in parser_gen:
        node = create_node(position, event, *data)

        # if we aren't already working on our return node,
        # then this is probably the one
        if ret is None:
            ret = node

        if event == parse.E_NEWLINE:
            # let the parser count lines for us
            continue

        elif event == parse.E_COMMENT:
            # don't bother representing comments in the AST
            continue

        elif event == parse.E_OPEN:
            stack.append(node)
            continue

        elif event == parse.E_DOT:
            stack[-1].proper = False
            continue

        elif event == parse.E_CLOSE:
            node = stack.pop()

        elif event in (parse.E_SHARP,
                       parse.E_QUOTE, parse.E_QUASI,
                       parse.E_UNQUOTE, parse.E_SPLICE):

            marked = compose(parser_gen)
            node.expression = marked

        elif event in (parse.E_SYMBOL, parse.E_NUMBER,
                       parse.E_STRING):
            pass

        # finished lists, literals should reach here
        assert(node is not None)
        if stack:
            stack[-1].members.append(node)
        elif ret:
            break

    return ret


def compose_from_str(src_str):
    pgen = parse.parse(StringIO(src_str))
    return compose(pgen)


def compose_all_from_str(src_str):
    pgen = parse.parse(StringIO(src_str))
    return iter(partial(compose, pgen), None)


#
# The end.
