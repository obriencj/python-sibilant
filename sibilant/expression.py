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
from .dispatch import Dispatch


class Expression(object):
    """
    Base class for sibilant expressions
    """

    __metaclass__ = ABCMeta


    def __init__(self, position, value):
        self.position = position
        self.value = value


    def __eq__(self, other):
        return ((type(self) is type(other)) and
                (self.position == other.position) and
                (self.value == other.value))


    def __ne__(self, other):
        return not (self.__eq__(other))


    def __repr__(self):
        data = (type(self).__name__,
                self.position,
                self.value)
        return "%s(position=%r,value=%r)" % data


class Variable(Expression):
    """
    A symbolic lookup of a value in the runtime namespace
    """

    def __repr__(self):
        data = (type(self).__name__,
                self.position,
                self.value)
        return "%s(position=%r,name=%r)" % data


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

        return "%s(position=%r,fun=%r,args=[%s])" % data


class Begin(Special):
    """
    Evaluate sub expressions in order
    """

    def __init__(self, position, *body):
        self.position = position
        self.body = list(body)


    def __eq__(self, other):
        return ((type(self) is type(other)) and
                (self.position == other.position) and
                (self.body == other.body))


    def __repr__(self):
        data = (type(self).__name__,
                self.position,
                ",".join(map(repr, self.body)))

        return "%s(position=%r,body=[%s])" % data


class Cond(Special):
    pass


class Define(Special):
    pass


class If(Special):
    pass


class Formal(object):
    """
    A named parameter to a function, or a name for a `let` binding
    """

    def __init__(self, position, name):
        self.position = position
        self.name = name


    def __eq__(self, other):
        return ((type(self) is type(other)) and
                (self.position == other.position) and
                (self.name == other.name))


    def __ne__(self, other):
        return not self.__eq__(other)


    def __repr__(self):
        data = (type(self).__name__,
                self.position,
                self.name)
        return "%s(position=%r,name=%r)" % data


class FormalsList(object):
    """
    List of Formals. Can be proper or improper

    Examples
    ========
    >>> FormalsList((2, 5), Formal((2, 6), "a"), rest=Formal((2, 9), "tail"))

    """

    def __init__(self, position, *frmls, **kwds):
        self.position = position
        self.frmls = list(frmls)
        self.rest = kwds.get('rest')


    def __eq__(self, other):
        return ((type(self) is type(other)) and
                (self.position == other.position) and
                (self.frmls == other.frmls) and
                (self.rest == other.rest))


    def __ne__(self, other):
        return not self.__eq__(other)


    def __repr__(self):
        if self.rest:
            data = (type(self).__name__,
                    self.position,
                    ",".join(map(repr, self.frmls)),
                    self.rest)
            msg = "%s(position=%r,args=[%s],rest=%r)"
        else:
            data = (type(self).__name__,
                    self.position,
                    ",".join(map(repr, self.frmls)))
            msg = "%s(position=%r,args=[%s])"

        return msg % data


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
                (self.formals == other.formals) and
                (self.body == other.body))


    def __repr__(self):
        data = (type(self).__name__,
                self.position,
                self.formals,
                ",".join(map(repr, self.body)))

        return "%s(position=%r,args=%r,body=[%s])" % data


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


    def transform(self, dispatcher):
        trans = dispatcher.dispatch
        self.pairs = [(k, trans(v)) for k, v in self.pairs]
        self.body = [trans(e) for e in self.body]


class Not(Special):

    def transform(self, dispatcher):
        self.expression = dispatcher.dispatch(self.expression)


class Print(Special):

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


    def __eq__(self, other):
        return ((type(self) is type(other)) and
                (self.position == other.position) and
                (self.var == other.var) and
                (self.val == other.val))


    def __repr__(self):
        data = (type(self).__name__,
                self.position,
                self.var,
                self.val)

        return "%s(position=%r,var=%r,value=%r" % data


    def transform(self, dispatcher):
        self.var = dispatcher.dispatch(self.var)
        self.val = dispatcher.dispatch(self.val)


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


def special_apply(disp, position, fun, *params):
    fun = disp.dispatch(fun)
    return Apply(position, fun, params)


def special_begin(disp, position, *body):
    return Begin(position, *(disp.dispatch(e) for e in body))


def special_cond(disp, position, *cond_pairs):
    d = disp.dispatch
    return Cond(position, *((d(con),d(exp)) for con,exp in cond_pairs))


def special_define(disp, name, val):
    return Define(position, Name(name.position, name.token),
                  disp.dispatch(val))


def special_lambda(disp, position, formals, *body):
    frml = FormalsList(formals.position)
    frml.frmls = [Formal(f.position, f.token) for f in formals.members]

    if not formals.proper:
        frml.rest = frml.frmls.pop()

    body = [disp.dispatch(e) for e in body]
    return Lambda(position, frml, *body)


specials = {
    "apply": special_apply,
    "begin": special_begin,
    "cond": special_cond,
    "define": special_define,
    "if": If,
    "lambda": special_lambda,
    "let": Let,
    "print": Print, # just for testing purposes
    "set!": Setf,
    "while": While,

    # these are actually builtins
    "not": Not,
    "+": Op_Add,
    "-": Op_Sub,
    "*": Op_Mult,
    "/": Op_Div,
    "and": Op_And,
    "or": Op_Or,
}


class NodeTranslator(Dispatch):
    """
    translates ast.Node instances into Expression instances
    """

    def dispatchList(self, node):
        pos = node.position
        fun = node.members[0]
        par = node.members[1:]

        expr = None
        if isinstance(fun, ast.Symbol):
            klass = specials.get(fun.token)
            if klass is not None:
                expr = klass(self, pos, *par)

        if expr is None:
            expr = special_apply(self, pos, fun, *par)

        return expr


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
    Translates an `ast.Node` instance into an `Expression`
    """

    eva = NodeTranslator()
    return eva.dispatch(node)


#
# The end.
