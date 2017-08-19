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
unittest for sibilant.compile

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


from contextlib import contextmanager
from fractions import Fraction as fraction
from functools import partial
from unittest import TestCase

import sibilant.builtins

from sibilant import (
    car, cdr, cons, nil,
    symbol, keyword, make_proper,
)

from sibilant.compiler import (
    is_macro, Macro,
    is_special, Special,
    iter_compile,
)

import dis


class Object(object):
    pass


def basic_env(**base):
    env = {"__builtins__": sibilant.builtins}
    env.update(base)
    return env


def compile_expr(src_str, **base):
    env = basic_env(**base)
    icode = iter_compile(src_str, env)
    code = next(icode)
    return partial(eval, code, env), env


def make_accumulator():
    accu = list()

    def accumulate(x):
        accu.append(x)
        return x

    return accu, accumulate


def make_raise_accumulator(excclass=Exception):
    accu = list()

    def accumulate(x):
        accu.append(x)
        raise excclass(x)

    return accu, accumulate


def make_manager():
    accumulator = list()

    def accu(val):
        accumulator.append(val)
        return val

    class Manager():
        def __init__(self, initial, enter, leave):
            self.enter = enter
            self.leave = leave
            accumulator.append(initial)

        def __enter__(self):
            accumulator.append(self.enter)
            return accu

        def __exit__(self, _a, _b, _c):
            accumulator.append(self.leave)
            return True

    return accumulator, Manager


class TestCompiler(TestCase):

    def test_global_symbol(self):
        src = "tacos"
        stmt, env = compile_expr(src, tacos=5)
        self.assertEqual(stmt(), 5)

        src = "tacos"
        stmt, env = compile_expr(src)
        self.assertRaises(NameError, stmt)


    def test_keyword(self):
        src = ":tacos"
        stmt, env = compile_expr(src)
        self.assertIs(stmt(), keyword("tacos"))

        src = "tacos:"
        stmt, env = compile_expr(src)
        self.assertIs(stmt(), keyword("tacos"))

        src = ":tacos:"
        stmt, env = compile_expr(src)
        self.assertIs(stmt(), keyword("tacos"))


    def test_bool(self):
        src = "True"
        stmt, env = compile_expr(src)
        self.assertIs(stmt(), True)

        src = "False"
        stmt, env = compile_expr(src)
        self.assertIs(stmt(), False)

        # this is testing that the Pythonic behavior of equating 0
        # with False, and 1 with True, is not impacting compilation
        # and storage of those constant values within the same code
        # block. this is a reproducer for a bug where the constant
        # values were being combined.

        src = "(make-list True 1 True 1 False 0 False 0)"
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertIs(res[0], True)
        self.assertIs(res[1], 1)
        self.assertIs(res[2], True)
        self.assertIs(res[3], 1)
        self.assertIs(res[4], False)
        self.assertIs(res[5], 0)
        self.assertIs(res[6], False)
        self.assertIs(res[7], 0)


    def test_number(self):
        src = "123"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 123)

        src = "-123"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), -123)

        src = "1/2"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), fraction(1, 2))

        src = "-1/2"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), fraction(-1, 2))

        src = "1.5"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1.5)

        src = ".5"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 0.5)

        src = "1."
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1.0)

        src = "-1.5"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), -1.5)

        src = "-1."
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), -1.0)

        src = "8+1j"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), complex("8+1j"))

        src = "3+i"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), complex("3+j"))

        src = "-1.1+2j"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), complex("-1.1+2j"))


    def test_string(self):
        src = '""'
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), "")

        src = '"hello world"'
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), "hello world")


    def test_cons(self):
        src = "(cons 1 2 3 nil)"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), cons(1, 2, 3, nil))


    def test_dot(self):
        src = "'(1.4)"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), cons(1.4, nil))

        src = "'(1. 4)"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), cons(1.0, 4, nil))

        src = "'(1 .4)"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), cons(1, 0.4, nil))

        src = "'(1 . 4)"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), cons(1, 4))

        src = "'(1. . .4)"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), cons(1.0, 0.4))


    def test_quote_symbol(self):
        src = "'tacos"
        stmt, env = compile_expr(src, tacos=5)
        self.assertEqual(stmt(), symbol("tacos"))


    def test_quote_keyword(self):
        src = "':tacos"
        stmt, env = compile_expr(src, tacos=5)
        self.assertIs(stmt(), keyword("tacos"))

        src = "'tacos:"
        stmt, env = compile_expr(src, tacos=5)
        self.assertIs(stmt(), keyword("tacos"))

        src = "':tacos:"
        stmt, env = compile_expr(src, tacos=5)
        self.assertIs(stmt(), keyword("tacos"))


    def test_quote_list(self):
        src = "'()"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), nil)

        src = "'(())"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), cons(nil, nil))

        src = "'(1 2 3)"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), cons(1, 2, 3, nil))


    def test_attr(self):

        o = Object()
        o.foo = Object()
        o.foo.bar = Object()
        o.foo.bar.baz = 111

        src = """
        o.foo.bar.baz
        """
        stmt, env = compile_expr(src, o=o)
        res = stmt()
        self.assertEqual(res, 111)

        src = """
        (attr o.foo.bar baz)
        """
        stmt, env = compile_expr(src, o=o)
        res = stmt()
        self.assertEqual(res, 111)


    def test_set_attr(self):

        o = Object()
        o.foo = Object()
        o.foo.bar = Object()
        o.foo.bar.baz = 111

        src = """
        (set-attr o.foo.bar baz 999)
        """
        stmt, env = compile_expr(src, o=o)
        res = stmt()
        self.assertEqual(res, None)
        self.assertEqual(o.foo.bar.baz, 999)

        src = """
        (set-attr (attr o.foo bar) baz 888)
        """
        stmt, env = compile_expr(src, o=o)
        res = stmt()
        self.assertEqual(res, None)
        self.assertEqual(o.foo.bar.baz, 888)


#
# The end.