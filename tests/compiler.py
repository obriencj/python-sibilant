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


import dis

from fractions import Fraction as fraction
from functools import partial
from unittest import TestCase

import sibilant.builtins

from sibilant import car, cdr, cons, nil, symbol

from sibilant.compiler import (
    macro, is_macro, Macro,
    compile_from_str, compile_from_stream,
)


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
        self.assertEqual(setter(5), None)
        self.assertEqual(getter(), 5)


    def test_define(self):
        src = """
        (define tacos 100)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), None)
        self.assertEqual(env["tacos"], 100)

        stmt, env = compile_expr(src, tacos=5)
        self.assertEqual(stmt(), None)
        self.assertEqual(env["tacos"], 100)


    def test_defun(self):
        src = """
        (defun add_8 (x) (+ x 8))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), None)

        add_8 = env["add_8"]
        self.assertTrue(callable(add_8))
        self.assertEqual(add_8.__name__, "add_8")
        self.assertEqual(add_8(1), 9)
        self.assertEqual(add_8(2), 10)

        stmt, env = compile_expr(src, add_8=None)
        self.assertEqual(stmt(), None)

        add_8 = env["add_8"]
        self.assertTrue(callable(add_8))
        self.assertEqual(add_8.__name__, "add_8")
        self.assertEqual(add_8(1), 9)
        self.assertEqual(add_8(2), 10)


    def test_defmacro(self):
        src = """
        (defmacro swap_test (a b c)
          (cons c b a '()))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), None)

        swap_test = env["swap_test"]
        self.assertTrue(isinstance(swap_test, Macro))
        self.assertTrue(is_macro(swap_test))
        self.assertEqual(swap_test.__name__, "swap_test")

        self.assertRaises(TypeError, swap_test, 1, 2, 3)

        self.assertEqual(swap_test.expand(1, 2, 3),
                         cons(3, 2, 1, nil))

        src = """
        (swap_test 'world 'hello cons)
        """
        # compiles to equivalent of (cons 'hello 'world)
        stmt, env = compile_expr(src, swap_test=swap_test)
        self.assertEqual(stmt(), cons(symbol("hello"), symbol("world")))


    def test_cond(self):
        src = """
        (cond)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, None)

        src = """
        (cond
          (True 100))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, 100)

        src = """
        (cond
          (else 100))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, 100)

        src = """
        (cond
          (False 100)
          (True 101)
          (else 102))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, 101)

        src = """
        (cond
          (False 100)
          (False 101)
          (else 102))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, 102)

        src = """
        (+ (cond
             (False 100)
             (nil 101)
             (else 102)) 100)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, 202)

        src = """
        (cond
          (False 100)
          (False 101)
          (else (cond (False 102)
                      (else 103))))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, 103)


    def test_quasiquote(self):
        src = """
        `(1 2 3 ,4 ,5)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, cons(1, 2, 3, 4, 5, nil))

        src = """
        `(1 2 3 ,A ,B Z)
        """
        stmt, env = compile_expr(src, A=4, B=5)
        res = stmt()
        self.assertEqual(res, cons(1, 2, 3, 4, 5, symbol("Z"), nil))

        src = """
        `(1 2 ,@A ,B)
        """
        stmt, env = compile_expr(src, A=cons(3, 4, nil), B=5)
        res = stmt()
        self.assertEqual(res, cons(1, 2, 3, 4, 5, nil))

        src = """
        `(1 2 ,@A)
        """
        stmt, env = compile_expr(src, A=range(3,6))
        res = stmt()
        self.assertEqual(res, cons(1, 2, 3, 4, 5, nil))

        src = """
        `(1 2 ,@(range 3 6))
        """
        stmt, env = compile_expr(src, range=range)
        res = stmt()
        self.assertEqual(res, cons(1, 2, 3, 4, 5, nil))

        src = """
        `(1 2 ,(foo 3 6))
        """
        stmt, env = compile_expr(src, foo=lambda a,b: list(range(a,b)))
        res = stmt()
        self.assertEqual(res, cons(1, 2, [3, 4, 5], nil))


#
# The end.
