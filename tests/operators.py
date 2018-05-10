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
unittest for sibilant.compile.operators

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


from unittest import TestCase

from sibilant import symbol, cons, nil, is_nil
from sibilant.compiler import (
    Special, is_special,
    Macro, is_macro,
)

from . import (
    compile_expr_bootstrap, make_accumulator, make_raise_accumulator,
    make_manager,
)


compile_expr = compile_expr_bootstrap


class BinaryOperators(TestCase):

    def test_and(self):
        # this tests the compiled form of `and`

        src = "(operator? and)"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (and 1 2 3)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 3)

        src = """
        (and 1 0 3)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 0)

        src = """
        (and)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        accu1, good_guy = make_accumulator()
        src = """
        (and (good_guy 1) (good_guy 0))
        """
        stmt, env = compile_expr(src, good_guy=good_guy)
        self.assertEqual(stmt(), 0)
        self.assertEqual(accu1, [1, 0])

        accu1, good_guy = make_accumulator()
        src = """
        (and (good_guy 0) (good_guy 1))
        """
        stmt, env = compile_expr(src, good_guy=good_guy)
        self.assertEqual(stmt(), 0)
        self.assertEqual(accu1, [0])

        accu1, good_guy = make_accumulator()
        src = """
        (and (good_guy nil) (good_guy None) (good_guy False))
        """
        stmt, env = compile_expr(src, good_guy=good_guy)
        self.assertEqual(stmt(), nil)
        self.assertEqual(accu1, [nil])


    def test_apply_and(self):
        # this tests the run-time application of `and` as a function

        src = """
        (apply and `(1 2 3))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 3)

        src = """
        (apply and `(1 0 3))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 0)

        src = """
        (apply and)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        accu1, good_guy = make_accumulator()
        src = """
        (apply and `(,(good_guy 1) ,(good_guy 0)))
        """
        stmt, env = compile_expr(src, good_guy=good_guy)
        self.assertEqual(stmt(), 0)
        self.assertEqual(accu1, [1, 0])

        accu1, good_guy = make_accumulator()
        src = """
        (apply and `(,(good_guy 0) ,(good_guy 1)))
        """
        stmt, env = compile_expr(src, good_guy=good_guy)
        self.assertEqual(stmt(), 0)
        self.assertEqual(accu1, [0, 1])

        accu1, good_guy = make_accumulator()
        src = """
        (apply and
               `(,(good_guy nil) ,(good_guy None) ,(good_guy False)))
        """
        stmt, env = compile_expr(src, good_guy=good_guy)
        self.assertEqual(stmt(), nil)
        self.assertEqual(accu1, [nil, None, False])


    def test_or(self):
        # this tests the compiled form of `or`

        src = "(operator? or)"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (or 1 2 3)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1)

        src = """
        (or 0 0 3)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 3)

        src = """
        (or)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), False)

        accu1, good_guy = make_accumulator()
        src = """
        (or (good_guy 1) (good_guy 0))
        """
        stmt, env = compile_expr(src, good_guy=good_guy)
        self.assertEqual(stmt(), 1)
        self.assertEqual(accu1, [1])

        accu1, good_guy = make_accumulator()
        src = """
        (or (good_guy 0) (good_guy 1))
        """
        stmt, env = compile_expr(src, good_guy=good_guy)
        self.assertEqual(stmt(), 1)
        self.assertEqual(accu1, [0, 1])

        accu1, good_guy = make_accumulator()
        src = """
        (or (good_guy 0) (good_guy None)
            (good_guy False) (good_guy nil))
        """
        stmt, env = compile_expr(src, good_guy=good_guy)
        self.assertEqual(stmt(), nil)
        self.assertEqual(accu1, [0, None, False, nil])


    def test_apply_or(self):
        # this tests the run-time application of `or` as a function

        src = """
        (apply or `(1 2 3))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1)

        src = """
        (apply or `(0 0 3))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 3)

        src = """
        (apply or `())
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), False)

        accu1, good_guy = make_accumulator()
        src = """
        (apply or `(,(good_guy 1) ,(good_guy 0)))
        """
        stmt, env = compile_expr(src, good_guy=good_guy)
        self.assertEqual(stmt(), 1)
        self.assertEqual(accu1, [1, 0])
        # note: different semantice when used as a runtime function,
        # no way to stop evaluation of arguments, because arguments
        # are collected prior to invocation

        accu1, good_guy = make_accumulator()
        src = """
        (apply or `(,(good_guy 0) ,(good_guy 1)))
        """
        stmt, env = compile_expr(src, good_guy=good_guy)
        self.assertEqual(stmt(), 1)
        self.assertEqual(accu1, [0, 1])

        accu1, good_guy = make_accumulator()
        src = """
        (apply or
               `(,(good_guy 0) ,(good_guy None)
                 ,(good_guy False) ,(good_guy nil)))
        """
        stmt, env = compile_expr(src, good_guy=good_guy)
        self.assertEqual(stmt(), nil)
        self.assertEqual(accu1, [0, None, False, nil])


    def test_add(self):
        # this tests the compiled form of `+`

        src = "(operator? +)"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (+)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (+ 1)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1)

        src = """
        (+ -1)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), -1)

        src = """
        (+ 1 2)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 3)

        src = """
        (+ 1 2 3)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 6)

        src = """
        (+ 1 2 -3)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 0)


    def test_apply_add(self):
        # this tests the run-time application of `+` as a function

        src = """
        (apply + `(1))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1)

        src = """
        (apply + `(-1))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), -1)

        src = """
        (apply + `(1 2))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 3)

        src = """
        (apply + `(1 2 3))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 6)

        src = """
        (apply + `(1 2 -3))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 0)


    def test_sub(self):
        # this tests the compiled form of `-`

        src = "(operator? -)"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (-)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (- 1)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), -1)

        src = """
        (- -1)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1)

        src = """
        (- 2 1)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1)

        src = """
        (- 99 1 1)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 97)

        src = """
        (- 99 1 -2)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 100)


    def test_apply_sub(self):
        # this tests the run-time application of `-` as a function

        src = """
        (apply - `(1))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), -1)

        src = """
        (apply - `(-1))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1)

        src = """
        (apply - `(2 1))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1)

        src = """
        (apply - `(99 1 1))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 97)

        src = """
        (apply - `(99 1 -2))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 100)


    def test_mult(self):

        src = "(operator? *)"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (*)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (* 999)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 999)

        src = """
        (* 5 2)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 10)

        src = """
        (* -5 2)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), -10)

        src = """
        (* -5 -2)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 10)

        src = """
        (* 1 2 3 -4)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), -24)

        src = """
        (* "TACOS " 2)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), "TACOS TACOS ")


    def test_apply_mult(self):

        src = """
        (apply * '(999))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 999)

        src = """
        (apply * '(5 2))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 10)

        src = """
        (apply * '(-5 2))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), -10)

        src = """
        (apply * '(-5 -2))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 10)

        src = """
        (apply * '(1 2 3 -4))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), -24)

        src = """
        (apply * '("TACOS " 2))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), "TACOS TACOS ")


    def test_div(self):

        src = "(operator? /)"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (/)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (/ 2)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 0.5)

        src = """
        (/ 10 2)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 5)

        src = """
        (/ -10 2)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), -5)

        src = """
        (/ -10 -2)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 5)

        src = """
        (/ -24 4 2)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), -3)


    def test_apply_div(self):

        src = """
        (apply / '(2))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 0.5)

        src = """
        (apply / '(10 2))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 5)

        src = """
        (apply / '(-10 2))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), -5)

        src = """
        (apply / '(-10 -2))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 5)

        src = """
        (apply / '(-24 4 2))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), -3)


    def test_floordiv(self):

        src = "(operator? //)"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (//)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (// 2)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertIs(res, 0)

        src = """
        (// 11 2)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 5)

        src = """
        (// -11 2)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), -6)

        src = """
        (// -11 -2)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 5)

        src = """
        (// 25 4 2)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 3)

        src = """
        (// -25 4 2)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), -4)


    def test_apply_floordiv(self):

        src = """
        (apply // '(2))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertIs(res, 0)

        src = """
        (apply // '(11 2))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 5)

        src = """
        (apply // '(-11 2))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), -6)

        src = """
        (apply // '(-11 -2))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 5)

        src = """
        (apply // '(25 4 2))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 3)

        src = """
        (apply // '(-25 4 2))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), -4)


    def test_power(self):

        src = "(operator? **)"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (** 1)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (** 1 2 3)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (** 2 2)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 4)

        src = """
        (** 4 1/2)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 2.0)

        src = """
        (** 4 0.5)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 2.0)


    def test_apply_power(self):

        src = """
        (apply ** '(2 2))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 4)

        src = """
        (apply ** `(4 ,1/2))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 2.0)

        src = """
        (apply ** '(4 0.5))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 2.0)


    def test_modulo(self):

        src = "(operator? %)"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (% 1)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (% 1 2 3)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (% 5 2)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1)

        src = """
        (% "first %s second %s third %s" (build-tuple 3 2 1))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), "first 3 second 2 third 1")


    def test_apply_modulo(self):

        src = """
        (apply % '(5 2))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1)

        src = """
        (apply % `("first %s second %s third %s" ,(build-tuple 3 2 1)))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), "first 3 second 2 third 1")


    def test_lshift(self):

        src = "(operator? <<)"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (<< 1)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (<< 1 2 3)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (<< 5 1)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 10)

        src = """
        (<< 1 0)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1)

        src = """
        (<< 1 1)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 2)

        src = """
        (<< 1 2)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 4)

        src = """
        (<< 1 3)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 8)

        src = """
        (<< 1 4)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 16)


    def test_apply_lshift(self):

        src = """
        (apply << '(5 1))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 10)

        src = """
        (apply << '(1 0))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1)

        src = """
        (apply << '(1 1))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 2)

        src = """
        (apply << '(1 2))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 4)

        src = """
        (apply << '(1 3))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 8)

        src = """
        (apply << '(1 4))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 16)


    def test_rshift(self):

        src = "(operator? >>)"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (>> 1)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (>> 1 2 3)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (>> 10 1)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 5)

        src = """
        (>> 1 1)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 0)

        src = """
        (>> 2 1)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1)

        src = """
        (>> 4 2)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1)

        src = """
        (>> 8 3)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1)

        src = """
        (>> 16 4)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1)


    def test_apply_rshift(self):

        src = """
        (apply >> '(10 1))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 5)

        src = """
        (apply >> '(1 1))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 0)

        src = """
        (apply >> '(2 1))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1)

        src = """
        (apply >> '(4 2))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1)

        src = """
        (apply >> '(8 3))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1)

        src = """
        (apply >> '(16 4))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1)


    def test_bitwise_and(self):

        src = "(operator? &)"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (&)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (& 1)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (& 3 11 23)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 3)

        src = """
        (& 5 3)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1)

        src = """
        (& 11 7)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 3)

        src = """
        (& 5 0)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 0)


    def test_apply_bitwise_and(self):
        src = """
        (apply & '(1))
        """
        stmt, env = compile_expr(src)
        self.assertRaises(TypeError, stmt)

        src = """
        (apply & '(5 3))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1)

        src = """
        (apply & '(5 3))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1)

        src = """
        (apply & '(11 7))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 3)

        src = """
        (apply & '(5 0))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 0)


    def test_bitwise_or(self):

        src = "(operator? |)"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (|)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (| 1)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (| 5 8 19)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 31)

        src = """
        (| 5 3)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 7)

        src = """
        (| 11 7)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 15)

        src = """
        (| 5 0)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 5)


    def test_apply_bitwise_or(self):

        src = """
        (apply | '(5 3))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 7)

        src = """
        (apply | '(5 8 19))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 31)

        src = """
        (apply | '(11 7))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 15)

        src = """
        (apply | '(5 0))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 5)


    def test_bitwise_xor(self):

        src = "(operator? ^)"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (^)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (^ 1)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (^ 5 3)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 6)

        src = """
        (^ 3 5 9)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 15)

        src = """
        (^ 11 7)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 12)

        src = """
        (^ 5 0)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 5)

        src = """
        (^ 5 5)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 0)


    def test_apply_bitwise_xor(self):

        src = """
        (apply ^ '(5 3))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 6)

        src = """
        (apply ^ '(3 5 9))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 15)

        src = """
        (apply ^ '(11 7))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 12)

        src = """
        (apply ^ '(5 0))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 5)

        src = """
        (apply ^ '(5 5))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 0)


class UnaryOperators(TestCase):

    def test_not(self):

        src = """
        (operator? not)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (not)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (not 1 2)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (not True)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), False)

        src = """
        (not 1)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), False)

        src = """
        (not '(tacos))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), False)

        src = """
        (not (build-list 0))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), False)

        src = """
        (not False)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (not 0)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (not '())
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (not (build-list))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)


    def test_apply_not(self):

        src = """
        (apply not '(,True))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), False)

        src = """
        (apply not '(1))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), False)

        src = """
        (apply not '((tacos)))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), False)

        src = """
        (apply not `(,(build-list 0))))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), False)

        src = """
        (apply not `(,False))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (apply not '(0))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (apply not '(()))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (apply not `(,(list)))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)


    def test_invert(self):

        src = """
        (operator? ~)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (~)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (~ 1 2)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (~ 0)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), -1)

        src = """
        (~ -1)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 0)

        src = """
        (~ 1)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), -2)

        src = """
        (~ -2)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1)

        src = """
        (~ 0xff)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), -256)

        src = """
        (~ (~ 999))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 999)


    def test_iter(self):

        src = """
        (operator? iter)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (iter)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (iter 1 2)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (iter X)
        """
        stmt, env = compile_expr(src, X=[1,2,3,4])
        res = stmt()
        self.assertEqual(type(res), type(iter([])))
        self.assertEqual(list(res), [1, 2, 3, 4])
        self.assertIs(iter(res), res)

        src = """
        (let ((Y (iter X)))
          (next Y))
        """
        stmt, env = compile_expr(src, X=[1,2,3,4])
        res = stmt()
        self.assertEqual(res, 1)

        src = """
        (let ((Y (iter X)))
          (next Y)
          (next Y))
        """
        stmt, env = compile_expr(src, X=[1,2,3,4])
        res = stmt()
        self.assertEqual(res, 2)


class Comparators(TestCase):

    def test_eq(self):
        src = """
        (eq 1 1)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (eq 0 1)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (== 1 1)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (== 0 1)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)


    def test_apply_eq(self):
        src = """
        (apply eq '(1 1))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (apply eq '(0 1))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (apply == '(1 1))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (apply == '(0 1))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)


    def test_not_eq(self):
        src = """
        (not-eq 1 1)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (not-eq 0 1)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (!= 1 1)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (!= 0 1)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)


    def test_apply_not_eq(self):
        src = """
        (apply not-eq '(1 1))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (apply not-eq '(0 1))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (apply != '(1 1))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (apply != '(0 1))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)


    def test_in(self):
        src = """
        (in 1 X)
        """
        stmt, env = compile_expr(src, X=[0, 1, 2])
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (in 9 X)
        """
        stmt, env = compile_expr(src, X=[0, 1, 2])
        res = stmt()
        self.assertEqual(res, False)


    def test_apply_in(self):
        src = """
        (apply in `(1 ,X))
        """
        stmt, env = compile_expr(src, X=[0, 1, 2])
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (apply in `(9 ,X))
        """
        stmt, env = compile_expr(src, X=[0, 1, 2])
        res = stmt()
        self.assertEqual(res, False)


    def test_contains(self):
        src = """
        (contains X 1)
        """
        stmt, env = compile_expr(src, X=[0, 1, 2])
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (contains X 9)
        """
        stmt, env = compile_expr(src, X=[0, 1, 2])
        res = stmt()
        self.assertEqual(res, False)


    def test_apply_contains(self):
        src = """
        (apply contains `(,X 1))
        """
        stmt, env = compile_expr(src, X=[0, 1, 2])
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (apply contains `(,X 9))
        """
        stmt, env = compile_expr(src, X=[0, 1, 2])
        res = stmt()
        self.assertEqual(res, False)


    def test_not_in(self):
        src = """
        (not-in 1 X)
        """
        stmt, env = compile_expr(src, X=[0, 1, 2])
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (not-in 9 X)
        """
        stmt, env = compile_expr(src, X=[0, 1, 2])
        res = stmt()
        self.assertEqual(res, True)


    def test_apply_not_in(self):
        src = """
        (apply not-in `(1 ,X))
        """
        stmt, env = compile_expr(src, X=[0, 1, 2])
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (apply not-in `(9 ,X))
        """
        stmt, env = compile_expr(src, X=[0, 1, 2])
        res = stmt()
        self.assertEqual(res, True)


    def test_not_contains(self):
        src = """
        (not-contains X 1)
        """
        stmt, env = compile_expr(src, X=[0, 1, 2])
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (not-contains X 9)
        """
        stmt, env = compile_expr(src, X=[0, 1, 2])
        res = stmt()
        self.assertEqual(res, True)


    def test_apply_not_contains(self):
        src = """
        (apply not-contains `(,X 1))
        """
        stmt, env = compile_expr(src, X=[0, 1, 2])
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (apply not-contains `(,X 9))
        """
        stmt, env = compile_expr(src, X=[0, 1, 2])
        res = stmt()
        self.assertEqual(res, True)


    def test_is(self):
        o1 = object()
        o2 = object()

        src = """
        (is X Y)
        """
        stmt, env = compile_expr(src, X=o1, Y=o1)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (is X Y)
        """
        stmt, env = compile_expr(src, X=o1, Y=o2)
        res = stmt()
        self.assertEqual(res, False)


    def test_apply_is(self):
        o1 = object()
        o2 = object()

        src = """
        (apply is `(,X ,Y))
        """
        stmt, env = compile_expr(src, X=o1, Y=o1)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (apply is `(,X ,Y))
        """
        stmt, env = compile_expr(src, X=o1, Y=o2)
        res = stmt()
        self.assertEqual(res, False)


    def test_is_not(self):
        o1 = object()
        o2 = object()

        src = """
        (is-not X Y)
        """
        stmt, env = compile_expr(src, X=o1, Y=o1)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (is-not X Y)
        """
        stmt, env = compile_expr(src, X=o1, Y=o2)
        res = stmt()
        self.assertEqual(res, True)


    def test_apply_is_not(self):
        o1 = object()
        o2 = object()

        src = """
        (apply is-not `(,X ,Y))
        """
        stmt, env = compile_expr(src, X=o1, Y=o1)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (apply is-not `(,X ,Y))
        """
        stmt, env = compile_expr(src, X=o1, Y=o2)
        res = stmt()
        self.assertEqual(res, True)


    def test_lt(self):
        src = """
        (< 1 2)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (lt 1 2)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (< 99 99)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (lt 99 99)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (< 2 1)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (lt 2 1)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)


    def test_apply_lt(self):
        src = """
        (apply < '(1 2))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (apply lt '(1 2))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (apply < '(99 99))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (apply lt '(99 99))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (apply < '(2 1))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (apply lt '(2 1))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)


    def test_le(self):
        src = """
        (<= 1 2)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (le 1 2)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (<= 99 99)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (le 99 99)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (<= 2 1)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (le 2 1)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)


    def test_apply_le(self):
        src = """
        (apply <= '(1 2))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (apply le '(1 2))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (apply <= '(99 99))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (apply le '(99 99))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (apply <= '(2 1))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (apply le '(2 1))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)


    def test_gt(self):
        src = """
        (> 1 2)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (gt 1 2)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (> 99 99)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (gt 99 99)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (> 2 1)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (gt 2 1)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)


    def test_apply_gt(self):
        src = """
        (apply > '(1 2))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (apply gt '(1 2))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (apply > '(99 99))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (apply gt '(99 99))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (apply > '(2 1))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (apply gt '(2 1))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)


    def test_ge(self):
        src = """
        (>= 1 2)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (ge 1 2)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (>= 99 99)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (ge 99 99)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (>= 2 1)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (ge 2 1)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)


    def test_apply_ge(self):
        src = """
        (apply >= '(1 2))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (apply ge '(1 2))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (apply >= '(99 99))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (apply ge '(99 99))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (apply >= '(2 1))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)


class TypeBuilders(TestCase):

    def test_build_dict(self):
        src = """
        (#dict)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertIs(type(res), dict)
        self.assertEqual(res, dict())

        src = """
        (#dict ("foo" 1)
               ("bar" 2))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertIs(type(res), dict)
        self.assertEqual(res, dict(foo=1, bar=2))


    def test_build_list(self):
        src = """
        (#list)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertIs(type(res), list)
        self.assertEqual(res, list())

        src = """
        (#list 1 2 3)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertIs(type(res), list)
        self.assertEqual(res, [1, 2, 3])


    def test_build_set(self):
        src = """
        (#set)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertIs(type(res), set)
        self.assertEqual(res, set())

        src = """
        (#set 1 2 3)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertIs(type(res), set)
        self.assertEqual(res, {1, 2, 3})


    def test_build_tuple(self):
        src = """
        (#tuple)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertIs(type(res), tuple)
        self.assertEqual(res, tuple())

        src = """
        (#tuple 1 2 3)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertIs(type(res), tuple)
        self.assertEqual(res, (1, 2, 3))


    def test_build_str(self):
        src = """
        (#str)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertIs(type(res), str)
        self.assertEqual(res, "")

        src = """
        (#str "a" "b" "c")
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertIs(type(res), str)
        self.assertEqual(res, "abc")

        src = """
        (lambda (X) (#str "a" "b" X "c"))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        code = res.__code__
        self.assertEqual(code.co_consts[0], None)
        self.assertEqual(code.co_consts[1], "ab")
        self.assertEqual(code.co_consts[2], "c")
        self.assertEqual(res(""), "abc")
        self.assertEqual(res(" "), "ab c")


    def test_build_slice(self):
        src = """
        (#slice 0 1)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, slice(0, 1))

        src = """
        (#slice 0 1 -1)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, slice(0, 1, -1))


class Format(TestCase):

    def test_format(self):
        src = """(format 100)"""
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, "100")

        src = """(format 100 "05")"""
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, "00100")


    def test_apply_format(self):
        src = """(apply format '(100))"""
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, "100")

        src = """(apply format '(100 "05"))"""
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, "00100")


#
# The end.
