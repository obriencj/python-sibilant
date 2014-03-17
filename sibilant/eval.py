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
Evaluated expressions for Sibilant

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


from abc import ABCMeta, abstractmethod
from functools import partial

import sibilant.ast as ast


class Expression(object):
    """
    Base class for sibilant expressions
    """

    __metaclass__ = ABCMeta

    @abstractmethod
    def __eval__(self, k):
        pass


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


class Begin(Special):
    """
    Evaluate sub expressions in order
    """

    def __init__(self, position, *body):
        self.position = position
        self.body = list(body)


    def transform(self):
        self.body = [e.translate() for e in self.body]


    def __eq__(self, other):
        return ((type(self) is type(other)) and
                (self.body == other.body))


    def __repr__(self):
        data = (type(self).__name__,
                self.position,
                ",".join(map(repr, self.body)))

        return "%s(position=%i,body=[%s])" % data


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


    def transform(self):
        self.body = [e.translate() for e in self.body]


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
        return List(pair.position, pair.members[0], pair.members[1].translate())


    def transform(self):
        self.pairs = [(k, v.translate()) for k, v in self.pairs]
        self.body = [e.translate() for e in self.body]


class Not(Special):

    def __init__(self, position, expr):
        self.position = position
        self.expression = expr


    def transform(self):
        self.expression = self.expression.translate()


class Print(Special):

    def __init__(self, position, expr):
        self.position = position
        self.expression = expr


    def transform(self):
        self.expression = self.expression.translate()


class Set(Special):
    """
    (setf symbol value)
    (setf symbol address value)
    """

    def __init__(self, position, var, val):
        self.position = position
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


builtins = {
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


class TrampolineCall(BaseException):
    """
    Signals evaluate to bounce the call stack off and continue from
    the current continuation.
    """

    pass


class ContinuationCall(TrampolineCall):
    """
    A TrampolineCall triggered by an invocation of call/cc
    """

    pass


def evaluate_and_return(expr):
    """
    evaluate `expr` with a continuation that simply collects the
    results and then returns them from this function.
    """

    def _return(value):
        # raise a trampoline call, since we know this is the last
        # continuation, and it'll save us the return trip up the call
        # stack
        raise TrampolineCall(lambda: value).with_stacktrace(None)

    return evaluate(expr, _return)


def evaluate(expr, k_cont):
    """
    Enters into the evaluation mode and calls `expr` with the
    continuation function `k_cont`

    Parameters
    ----------
    expr : `Expression`
      the expression to evaluate
    k_cont : `function(value)`
      the continuation to pass to the expression

    Returns
    -------
    value
      the result of `k_cont` when evaluation has completed
    """

    work = partial(expr.__eval__, k)
    while True:
        try:
            result = work()
        except TrampolineCall as tc:
            work = tc.args[0]
        else:
            return result


def compile_node(node):
    """
    Compiles an ast.Node instance into an Expression
    """

    pass


def compile_from_stream(stream):
    return compile_node(compose_from_stream(stream))


def compile_from_str(src_str):
    return compile_node(compose_from_str(src_str))


#
# The end.
