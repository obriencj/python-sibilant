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
Expression trees for Sibilant

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


from abc import ABCMeta, abstractmethod

import sibilant.ast as ast


class Expression(object):
    """
    Base class for sibilant expressions
    """

    __metaclass__ = ABCMeta

    def transform(self, dispatcher):
        pass


class Variable(Expression):
    """
    A symbolic lookup of a value in the runtime namespace
    """

    def __init__(self, position, name):
        self.position = position
        self.name = name


class Literal(Expression):
    """
    Base class for expressions which have a literal value
    """

    __metaclass__ = ABCMeta


class Special(Expression):
    """
    parent class for all special form expressions
    """

    __metaclass__ = ABCMeta


class Apply(Special):
    """
    Application of a function
    """

    def __init__(self, position, fun, *args):
        self.position = position
        self.function = fun
        self.args = args


    def __eq__(self, other):
        return ((type(self) is type(other)) and
                (self.position == other.position) and
                (self.function == other.function) and
                (self.args == other.args))


    def __repr__(self):
        data = (type(self).__name__,
                self.position,
                self.function,
                ",".join(map(repr, self.args)))

        return "%s(position=%i,fun=%r,args=[%s])" % data


    def transform(self, dispatcher):
        self.function = dispatcher.dispatch(self.function)
        self.args = [dispatcher.dispatch(a) for a in self.args]


class Begin(Special):
    """
    Evaluate sub expressions in order
    """

    def __init__(self, position, *body):
        self.position = position
        self.body = list(body)


    def __eq__(self, other):
        return ((type(self) is type(other)) and
                (self.body == other.body))


    def __repr__(self):
        data = (type(self).__name__,
                self.position,
                ",".join(map(repr, self.body)))

        return "%s(position=%i,body=[%s])" % data


    def transform(self, dispatcher):
        self.body = [dispatcher.dispatch(b) for b in self.body]


class Cond(Special):
    pass


class Define(Special):
    pass


class If(Special):
    pass


class Lambda(Special):
    """
    Function definition
    """

    def __init__(self, position, formals, *body):
        self.position = position
        self.formals = formals
        self.body = list(body)


    def __eq__(self, other):
        return ((type(self) is type(other)) and
                (self.position == other.position) and
                (self.body == other.body))


    def __repr__(self):
        data = (type(self).__name__,
                self.position,
                ",".join(map(repr, self.formals)),
                ",".join(map(repr, self.body)))

        return "%s(position=%i,formals=[%s],body=[%s])" % data


    def transform(self):
        self.body = [e.translate() for e in self.body]


class Let(Special):

    def __init__(self, position, x, *y):
        self.position = position

        if is_list(x):
            self.name = None
            self.pairs = [p.members for p in x.members]
            self.body = y

        elif is_token(x):
            self.name = x
            self.pairs = [p.members for p in y[0].members]
            self.body = y[1:]


    def translate_let_pair(self, pair):
        return List(pair.position, pair.members[0],
                    pair.members[1].translate())


    def transform(self):
        self.pairs = [(k, v.translate()) for k, v in self.pairs]
        self.body = [e.translate() for e in self.body]


class Not(Special):

    def __init__(self, position, expr):
        self.position = position
        self.expression = expr


    def transform(self, dispatcher):
        self.expression = dispatcher.dispatch(self.expression)


class Print(Special):

    def __init__(self, position, expr):
        self.position = position
        self.expression = expr


    def transform(self, dispatcher):
        self.expression = dispatcher.dispatch(self.expression)


class Setf(Special):
    """
    (set! symbol value)
    (set! (car symbol) value)
    (set! (cdr symbol) value)
    """

    def __init__(self, position, var, val):
        self.position = position

        self.var = var
        self.val = val


    def transform(self, dispatcher):
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
    "apply": Apply,

    "begin": Begin,
    "cond": Cond,
    "define": Define,
    "if": If,
    "lambda": Lambda,
    "let": Let,
    "print": Print, # just for testing purposes
    "set!": Setf,
    "while": While,

    "not": Not,
    "+": Op_Add,
    "-": Op_Sub,
    "*": Op_Mult,
    "/": Op_Div,
    "and": Op_And,
    "or": Op_Or,
}


class Number(Literal):
    pass


class Integer(Number):
    pass


class Decimal(Number):
    pass


class Fraction(Number):
    pass


class Complex(Number):
    pass


class Boolean(Literal):
    pass


class Character(Literal):
    pass


class Symbol(Literal):
    pass


class Nil(Literal):
    pass


class String(Literal):
    pass


class ExpressionTransformer(Dispatch):
    """
    transforms ast.Node instances into Expression instances
    """

    def dispatchList(self, node):
        pos = node.position
        fun = node.members[0]
        par = node.members[1:]

        tmp = None
        if isinstance(fun, ast.Symbol):
            klass = specials.get(fun.token)
            if klass is not None:
                tmp = klass(pos, *par)

        if tmp is None:
            tmp = Apply(pos, fun, *par)

        tmp.transform(self)
        return tmp


    def dispatchSymbol(self, node):
        return Variable(node.position, node.token)


    def dispatchNumber(self, node):
        pos = node.position
        tok = node.token

        def parse_num(stok):
            if '/' in tok:
                l, r = tok.split('/', 1)
                return Fraction(pos, parse_num(l), parse_num(r))
            elif tok[-1] in "ij":
                return Complex(pos, tok)
            elif '.' in tok:
                return Decimal(pos, tok)
            else:
                return Integer(pos, tok)

        return parse_num(tok)


    def dispatchString(self, node):
        return String(node.position, node.token)


    def dispatchSharp(self, node):
        pos = node.position
        sub = node.expression
        if isinstance(sub, ast.Symbol):
            ident = sub.token
            if ident in "ft":
                return Boolean(pos, ident == 't')
            elif ident in 'bodx':
                return Number(pos, ident)
            elif ident in 'ie':
                return Number(pos, ident)
            elif ident[0] == '\\':
                return Character(pos, ident)

        # fall-through from above
        raise SyntaxError(pos, "unkown sharp expression")


    def dispatchQuote(self, node):
        pass


    def dispatchQuasi(self, node):
        pass


    def dispatchUnquote(self, node):
        pass


    def dispatchSplice(self, node):
        pass



def translate_node(node):
    """
    Compiles an `Node` instance into an `Expression`
    """

    eva = ExpressionTransformer()
    return eva.transform(node)


#
# The end.
