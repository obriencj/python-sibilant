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

    def __init__(self, line=1):
        self.line = line

    def translate(self):
        return self

    def __ne__(self, other):
        return not (self == other)

    def __repr__(self):
        return "%s(line=%i)" % (type(self).__name__, self.line)

    def __str__(self):
        return repr(self)


class Comment(Node):
    """
    A comment
    """

    def __init__(self, line, txt):
        self.line = line
        self.text = txt

    def __eq__(self, other):
        return ((type(self) is type(other)) and
                (self.line == other.line) and
                (self.text == other.text))

    def __repr__(self):
        data = (type(self).__name__,
                self.line,
                self.text)

        return "%s(line=%i,txt=%r)" % data


class Expression(Node):
    pass


class Marked(Node):
    """
    Parent class for mark indicators which augment another expression
    """

    def __init__(self, line, expression=None):
        self.line = line
        self.expression = expression


    def __eq__(self, other):
        return ((type(self) is type(other)) and
                (self.line == other.line) and
                (self.expression == other.expression))


    def __repr__(self):
        data = (type(self).__name__,
                self.line,
                self.expression)

        return "%s(line=%i,%r)" % data


class Atom(Expression):
    """
    Parent class for single-token expressions
    """

    def __init__(self, line, token):
        self.line = line
        self.token = token


    def __eq__(self, other):
        return ((type(self) is type(other)) and
                (self.line == other.line) and
                (self.token == other.token))


    def __repr__(self):
        data = (type(self).__name__,
                self.line,
                self.token)
        return "%s(line=%i,%r)" % data


class List(Expression):
    """
    A collection of sub-expressions
    """

    def __init__(self, line, *members):
        self.line = line
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
                    tmp = klass(self.line, *param)
                    tmp.transform()
                    return tmp

            # if member[0] isn't a symbol, or isn't a symbol that is
            # considered special, it's a function application

            membs = [m.translate() for m in self.members]
            return Apply(self.line, fun, *param)

        else:
            return self


    def __repr__(self):
        data = (type(self).__name__,
                self.line,
                self.proper,
                ",".join(map(repr, self.members)))
        return "%s(line=%i,proper=%r,members=[%s])" % data


    def __eq__(self, other):
        return ((type(self) is type(other)) and
                (self.line == other.line) and
                (self.proper == other.proper) and
                (self.members == other.members))


is_list = typep(List)


# literals


class Symbol(Atom):
    pass


is_symbol = typep(Symbol)


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
                return Boolean(self.line, ident)
            elif ident in "bodx":
                # xxx
                return Number(self.line, self.expression)
            elif ident in "ie":
                # xxx
                return Number(self.line, self.expression)
            elif ident == "\\":
                # xxx
                return Character(self.line, self.expression)

        elif is_list(self.expression):
            return Vector(self.line, self.expression.members)

        else:
            pass


class Boolean(Atom):
    pass


class Character(Atom):
    pass


class Vector(Atom):
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


# specials


class Special(Expression):
    """
    parent class for all special form expressions
    """

    pass


class Apply(Special):

    def __init__(self, line, fun, *args):
        self.line = line
        self.function = fun
        self.args = args


    def __eq__(self, other):
        return ((type(self) is type(other)) and
                (self.line == other.line) and
                (self.function == other.function) and
                (self.args == other.args))


    def __repr__(self):
        data = (type(self).__name__,
                self.line,
                self.function,
                ",".join(map(repr, self.args)))

        return "%s(line=%i,fun=%r,args=[%s])" % data


class Begin(Special):

    def __init__(self, line, *body):
        self.line = line
        self.body = list(body)


    def transform(self):
        self.body = [e.translate() for e in self.body]


    def __eq__(self, other):
        return ((type(self) is type(other)) and
                (self.body == other.body))


    def __repr__(self):
        data = (type(self).__name__,
                self.line,
                ",".join(map(repr, self.body)))

        return "%s(line=%i,body=[%s])" % data


class Cond(Special):
    pass


class Define(Special):
    pass


class If(Special):
    pass


class Lambda(Special):

    def __init__(self, line, formals, *body):
        self.line = line
        self.formals = formals
        self.body = body


    def transform(self):
        self.body = [e.translate() for e in self.body]


class Let(Special):

    def __init__(self, line, x, *y):
        self.line = line

        if is_list(x):
            self.name = None
            self.pairs = [p.members for p in x.members]
            self.body = y

        elif is_token(x):
            self.name = x
            self.pairs = [p.members for p in y[0].members]
            self.body = y[1:]


    def translate_let_pair(self, pair):
        return List(pair.line, pair.members[0], pair.members[1].translate())


    def transform(self):
        self.pairs = [(k, v.translate()) for k, v in self.pairs]
        self.body = [e.translate() for e in self.body]


class Not(Special):

    def __init__(self, line, expr):
        self.line = line
        self.expression = expr


    def transform(self):
        self.expression = self.expression.translate()


class Print(Special):

    def __init__(self, line, expr):
        self.line = line
        self.expression = expr


    def transform(self):
        self.expression = self.expression.translate()


class Set(Special):

    def __init__(self, line, var, val):
        self.line = line
        self.var = var
        self.val = val


    def transform(self):
        self.val = self.val.translate()


class While(Special):
    pass


class Operator(Special):
    pass


class Op_Add(Operator):
    pass


class Op_Sub(Operator):
    pass


class Op_Mult(Operator):
    pass


class Op_Div(Operator):
    pass


class Op_And(Operator):
    pass


class Op_Or(Operator):
    pass


specials = {
    "begin": Begin,
    "cond": Cond,
    "define": Define,
    "if": If,
    "lambda": Lambda,
    "let": Let,
    "print": Print, # just for testing purposes
    "set!": Set,
    "while": While,
}


procedures = {
    "apply": Apply,
    "not": Not,

    "+": Op_Add,
    "-": Op_Sub,
    "*": Op_Mult,
    "/": Op_Div,
    "and": Op_And,
    "or": Op_Or,
}


def translate_special(listnode):
    # turn a List instance into one of the specials, calls translate on
    # any sub-expressions that might need it

    pass


def translate_quote(quotenode):
    # translates unquote and splice contents
    pass


def translate_sharp(sharpnode):
    # translates sharps into the appropriate subclasses and translates
    # vector items
    pass


def translate(node):
    return node.translate()


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


def create_node(line_no, event, *args):
    klass = klass_events.get(event)
    return klass(line_no, *args) if klass else None


def compose(parser_gen, starting_line=1):

    ret = None
    stack = []
    line_no = starting_line

    for event, *data in parser_gen:
        node = create_node(line_no, event, *data)

        if not ret:
            ret = node

        if event == parse.E_NEWLINE:
            line_no += 1

        elif event == parse.E_COMMENT:
            # don't bother representing comments in the AST
            pass

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

            marked = compose(parser_gen, line_no)
            node.expression = marked

        elif event in (parse.E_SYMBOL, parse.E_NUMBER,
                       parse.E_STRING):
            pass

        # now take node and stick it in the param slot for
        if stack:
            #print("stack", stack)
            stack[-1].members.append(node)

        else:
            break

    return ret


def compose_from_str(src_str, starting_line=1):
    pgen = parse.parse(StringIO(src_str))
    return compose(pgen, starting_line)


#
# The end.
