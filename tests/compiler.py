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


from fractions import Fraction as fraction
from functools import partial
from unittest import TestCase

import sibilant.builtins

from sibilant import car, cdr, cons, nil, symbol
from sibilant.compiler import *


def basic_env(**base):
    env = {"__builtins__": sibilant.builtins}
    env.update(base)
    return env


def compile_expr(src_str, **base):
    env = basic_env(**base)
    code = compile_from_str(src_str, env)
    return partial(eval, code, env), env


class TestCompiler(TestCase):

    def test_global_symbol(self):
        src = "tacos"
        stmt, env = compile_expr(src, tacos=5)
        self.assertEqual(stmt(), 5)

        src = "tacos"
        stmt, env = compile_expr(src)
        self.assertRaises(NameError, stmt)


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


    def test_quote_symbol(self):
        src = "'tacos"
        stmt, env = compile_expr(src, tacos=5)
        self.assertEqual(stmt(), symbol("tacos"))


    def test_cons(self):
        src = "(cons 1 2 3 nil)"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), cons(1, 2, 3, nil))


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


    def test_lambda(self):
        src = "(lambda () tacos)"
        stmt, env = compile_expr(src)
        fun = stmt()
        self.assertTrue(callable(fun))
        self.assertRaises(NameError, fun)

        src = "(lambda () tacos)"
        stmt, env = compile_expr(src, tacos=5)
        fun = stmt()
        self.assertTrue(callable(fun))
        self.assertEqual(fun(), 5)

        src = "(lambda (tacos) tacos)"
        stmt, env = compile_expr(src, tacos=5)
        fun = stmt()
        self.assertTrue(callable(fun))
        self.assertEqual(fun(1), 1)


    def test_make_adder(self):
        src = "(lambda (X) (lambda (Y) (+ X Y)))"
        stmt, env = compile_expr(src)
        make_adder = stmt()
        self.assertTrue(callable(make_adder))

        add_8 = make_adder(8)
        self.assertEqual(add_8(1), 9)

        hello = make_adder("hello ")
        self.assertEqual(hello("world"), "hello world")

        # just to show the cell wasn't polluted by the new make_adder
        # call
        self.assertEqual(add_8(2), 10)


    def test_let(self):
        src = "(let ((tacos 1) (beer 2)) (cons tacos beer))"
        stmt, env = compile_expr(src, tacos=5)
        self.assertEqual(stmt(), cons(1, 2))

        src = "(let ((tacos 1) (beer 2)) (cons tacos beer nil))"
        stmt, env = compile_expr(src, tacos=5)
        self.assertEqual(stmt(), cons(1, 2, nil))

        src = "(let () (cons tacos beer nil))"
        stmt, env = compile_expr(src, tacos=5, beer=9)
        self.assertEqual(stmt(), cons(5, 9, nil))

        src = "(let ((tacos 1)) (cons tacos beer))"
        stmt, env = compile_expr(src, tacos=5, beer=9)
        self.assertEqual(stmt(), cons(1, 9))


    def test_getter_setter(self):
        src = """
        (lambda (value)
          (cons (lambda () value)
                (lambda (v) (set! value v))))
        """

        stmt, env = compile_expr(src)

        getter_setter = stmt()
        self.assertTrue(callable(getter_setter))

        getter, setter = getter_setter(0)
        self.assertTrue(callable(getter))
        self.assertTrue(callable(setter))

        self.assertEqual(getter(), 0)
        #self.assertEqual(setter(5), 5)
        #self.assertEqual(getter(), 5)


#
# The end.
