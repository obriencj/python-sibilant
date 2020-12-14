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
unittest for sibilant.compile.specials

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


import dis

from functools import partial
from types import GeneratorType
from unittest import TestCase
from asynctest import TestCase as AsyncTestCase

from sibilant.lib import (
    symbol, keyword, cons, nil, is_nil,
    getderef, setderef, clearderef,
)

from sibilant.compiler import (
    Special, is_special,
    Macro, is_macro,
)

from . import (
    compile_expr_bootstrap, compile_dis_expr,
    make_accumulator, make_raise_accumulator,
    make_manager,
)


compile_expr = compile_expr_bootstrap


class Object(object):
    pass


class Lambda(TestCase):

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


    def test_getter_setter(self):
        src = """
        (lambda (value)
          (cons (lambda () value)
                (lambda (v) (setq value v))))
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


    def test_4(self):
        src = """
        (lambda (a b)
          (setq a ((lambda (x) (+ a x)) 9))
          (cons a b))
        """
        stmt, env = compile_expr(src)
        fun = stmt()
        res = fun(100, 200)
        self.assertTrue(res, cons(109, 200))

        src = """
        (lambda (a b)
          (setq b ((lambda (x) (+ b x)) 9))
          (cons a b))
        """
        stmt, env = compile_expr(src)
        fun = stmt()
        res = fun(100, 200)
        self.assertTrue(res, cons(100, 209))

        src = """
        (lambda (a b)
          (setq a ((lambda (x) (+ b x)) 9))
          (cons a b))
        """
        stmt, env = compile_expr(src)
        fun = stmt()
        res = fun(100, 200)
        self.assertTrue(res, cons(209, 200))

        src = """
        (lambda (a b)
          (setq b ((lambda (x) (+ a x)) 9))
          (cons a b))
        """
        stmt, env = compile_expr(src)
        fun = stmt()
        res = fun(100, 200)
        self.assertTrue(res, cons(100, 109))


    def test_3(self):
        src = """
        (lambda (a b)
          (lambda (x) (+ a x)))
        """
        stmt, env = compile_expr(src)
        fun = stmt()
        funx = fun(1, 9)
        self.assertTrue(funx(2), 3)

        src = """
        (lambda (a b)
          (lambda (x) (+ b x)))
        """
        stmt, env = compile_expr(src)
        fun = stmt()
        funx = fun(1, 9)
        self.assertTrue(funx(2), 11)


    def test_2(self):
        src = """
        (lambda (a b)
          (lambda (x)
           (lambda () (cons (+ a x) b))))
        """
        stmt, env = compile_expr(src)
        fun = stmt()
        funx = fun(1, 9)
        funy = funx(2)
        self.assertTrue(funy(), cons(3, 9))


    def test_1(self):
        src = """
        (lambda (a b)
          (lambda (x) (cons (+ a x) b)))
        """
        stmt, env = compile_expr(src)
        fun = stmt()
        funx = fun(1, 9)
        self.assertTrue(funx(2), cons(3, 9))


class Quasiquote(TestCase):


    def qq(self, src_str, expected, **env):
        stmt, env = compile_expr(src_str, **env)
        res = stmt()
        self.assertEqual(res, expected)


    def test_simple(self):
        self.qq("`1", 1)
        self.qq("`,1", 1)

        self.qq("`tacos",
                symbol("tacos"))

        self.qq("`:tacos",
                keyword("tacos"))

        self.qq("`nil",
                symbol("nil"))

        self.qq("`()",
                nil)

        self.qq("`(1 2 3 ,4 ,5)",
                cons(1, 2, 3, 4, 5, nil))

        self.qq("`(1 2 3 ,A ,B Z)",
                cons(1, 2, 3, 4, 5, symbol("Z"), nil),
                A=4, B=5)

        self.qq("`(1 2 ,(foo 3 6))",
                cons(1, 2, [3, 4, 5], nil),
                foo=lambda a, b: list(range(a,b)))


    def test_splice(self):

        self.qq("`(1 2 ,@A)",
                cons(1, 2, 3, 4, 5, nil),
                A=range(3, 6))

        self.qq("`(1 2 ,@A ,B)",
                cons(1, 2, 3, 4, 5, nil),
                A=cons(3, 4, nil), B=5)

        self.qq("`(1 2 ,@(range 3 6))",
                cons(1, 2, 3, 4, 5, nil))

        self.qq("`(,@foo)",
                cons(1, 2, 3, nil),
                foo=cons(1, 2, 3, nil))


    def test_nested(self):

        self.qq("``,(+ 1 2)",
                cons(symbol("quasiquote"),
                     cons(symbol("unquote"),
                          cons(symbol("+"),
                               1, 2,
                               nil),
                          nil),
                     nil))

        self.qq("`(1 2 `(foo ,(+ 1 2)))",
                cons(1, 2,
                     cons(symbol("quasiquote"),
                          cons(symbol("foo"),
                               cons(symbol("unquote"),
                                    cons(symbol("+"),
                                         1, 2,
                                         nil),
                                    nil),
                               nil),
                          nil),
                     nil))

        self.qq("`(1 2 `(foo ,,(+ 1 2)))",
                cons(1, 2,
                     cons(symbol("quasiquote"),
                          cons(symbol("foo"),
                               cons(symbol("unquote"),
                                    3,
                                    nil),
                               nil),
                          nil),
                     nil))

        self.qq("``,,(+ 1 2)",
                cons(symbol("quasiquote"),
                     cons(symbol("unquote"),
                          3,
                          nil),
                     nil))

        self.qq("`(1 `(bar ,@,foo))",
                cons(1,
                     cons(symbol("quasiquote"),
                          cons(symbol("bar"),
                               cons(symbol("unquote-splicing"),
                                    cons(1, 2, 3, nil),
                                    nil),
                               nil),
                          nil),
                     nil),
                foo=cons(1, 2, 3, nil))


    def test_improper(self):

        self.qq("`(1 . 2)",
                cons(1, 2))

        self.qq("`(1 . (2 3))",
                cons(1, 2, 3, nil))

        self.qq("`(1 . ,'(2 . 3))",
                cons(1, 2, 3))

        self.qq("`(1 . ,A)",
                cons(1, 2),
                A=2)

        self.qq("`(1 . ,A)",
                cons(1, 2, nil),
                A=cons(2, nil))

        self.qq("`(1 . ,A)",
                cons(1, 2, 3),
                A=cons(2, 3))


    def test_improper_splice(self):

        self.qq("`(1 . ,@A)",
                cons(1, 2, 3),
                A=cons(2, 3))

        self.qq("`(1 . ,@A)",
                cons(1, 2, 3, nil),
                A=cons(2, 3, nil))

        self.qq("`(1 . ,@A)",
                cons(1, 2, 3),
                A=range(2, 4))

        self.qq("`(1 . ,@A)",
                cons(1, nil),
                A=range(0, 0))

        self.qq("`(,@A . 4)",
                cons(1, 2, 3, 4),
                A=range(1, 4))


class CompilerSpecials(TestCase):


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


    def test_define(self):
        src = """
        (let ()
          (define tacos 100)
          tacos)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 100)
        self.assertNotIn("tacos", env)

        src = """
        (let ()
          (define tacos 100)
          tacos)
        """
        stmt, env = compile_expr(src, tacos=5)
        self.assertEqual(stmt(), 100)
        self.assertEqual(env["tacos"], 5)

        src = """
        (let ((beer 999))
          (define tacos beer)
          (define nachos 100)
          (+ nachos tacos))
        """
        stmt, env = compile_expr(src, tacos=5)
        self.assertEqual(stmt(), 1099)
        self.assertNotIn("beer", env)
        self.assertNotIn("nachos", env)


class SpecialLet(TestCase):


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


    def test_named_let(self):
        src = """
        (let fibonacci ((index CALC) (carry 0) (accu 1))
          (cond
            [(== index 0) carry]
            [else: (fibonacci (- index 1) accu (+ accu carry))]))
        """

        check = ((0, 0), (1, 1), (2, 1), (3, 2), (7, 13), (9, 34),
                 (10, 55), (11, 89), (20, 6765), )

        for index, expected in check:
            stmt, env = compile_expr(src, CALC=index)
            res = stmt()
            self.assertEqual(res, expected)


class SpecialCond(TestCase):


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
          (else: 100))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, 100)

        src = """
        (cond
          (False 100)
          (True 101)
          (else: 102))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, 101)

        src = """
        (cond
          (False 100)
          (False 101)
          (else: 102))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, 102)

        src = """
        (+ (cond
             (False 100)
             (nil 101)
             (else: 102)) 100)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, 202)

        src = """
        (cond
          (False 100)
          (False 101)
          (else: (cond (False 102)
                       (else: 103))))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, 103)


class SpecialWhile(TestCase):


    def test_while(self):
        src = """
        (while False (trigger))
        """
        trigger = partial(self.assertFalse, True, "trigger was called")
        stmt, env = compile_expr(src, trigger=trigger)
        res = stmt()
        self.assertEqual(res, None)

        src = """
        (while X
          (setq X (- X 1))
          (accumulate X))
        """
        data, accu = make_accumulator()
        stmt, env = compile_expr(src, X=10000, accumulate=accu)
        res = stmt()
        self.assertEqual(res, 0)
        self.assertEqual(data, list(range(9999, -1, -1)))


    def test_while_raise(self):
        src = """
        (while True (raise exc))
        """
        exc = Exception("I meant to do this")
        stmt, env = compile_expr(src, exc=exc)
        self.assertRaises(Exception, stmt)


class SpecialTry(TestCase):


    def test_try_noexc(self):

        accu, good_guy = make_accumulator()

        src = """
        (try
          (good_guy 567))
        """
        stmt, env = compile_expr(src, good_guy=good_guy)
        res = stmt()
        self.assertEqual(res, 567)
        self.assertEqual(accu, [567])


    def test_try_noexc_else(self):

        accu, good_guy = make_accumulator()

        src = """
        (try
          (good_guy 567)
          (else: (good_guy 888)))
        """
        stmt, env = compile_expr(src, good_guy=good_guy)
        res = stmt()
        self.assertEqual(res, 888)
        self.assertEqual(accu, [567, 888])


    def test_try_noexc_finally(self):

        accu, good_guy = make_accumulator()

        src = """
        (try
          (good_guy 567)
          (finally: (good_guy 999)))
        """
        stmt, env = compile_expr(src, good_guy=good_guy)
        res = stmt()
        self.assertEqual(res, 567)
        self.assertEqual(accu, [567, 999])


    def test_try_noexc_else_finally(self):

        accu, good_guy = make_accumulator()

        src = """
        (try
          (good_guy 567)
          (else: (good_guy 888))
          (finally: (good_guy 999)))
        """
        stmt, env = compile_expr(src, good_guy=good_guy)
        res = stmt()
        self.assertEqual(res, 888)
        self.assertEqual(accu, [567, 888, 999])


    def test_try_nocatch(self):

        accu, bad_guy = make_raise_accumulator()

        src = """
        (try
          (bad_guy 567))
        """
        stmt, env = compile_expr(src, bad_guy=bad_guy)
        self.assertRaises(Exception, stmt)
        self.assertEqual(accu, [567])


    def test_try_exc(self):

        accu1, bad_guy = make_raise_accumulator()
        accu2, good_guy = make_accumulator()

        src = """
        (try
          (bad_guy 567)
          ((Exception) (good_guy -111)))
        """
        stmt, env = compile_expr(src, **locals())
        res = stmt()
        self.assertEqual(res, -111)
        self.assertEqual(accu1, [567])
        self.assertEqual(accu2, [-111])


    def test_keyword_try_exc(self):

        accu1, bad_guy = make_raise_accumulator()
        accu2, good_guy = make_accumulator()

        src = """
        (try
          (bad_guy 567)
          (except: Exception (good_guy -111)))
        """
        stmt, env = compile_expr(src, **locals())
        res = stmt()
        self.assertEqual(res, -111)
        self.assertEqual(accu1, [567])
        self.assertEqual(accu2, [-111])


    def test_named_try_exc(self):

        accu1, bad_guy = make_raise_accumulator()
        accu2, good_guy = make_accumulator()

        src = """
        (try
          (bad_guy 567)
          ((e Exception) (good_guy -111)))
        """
        stmt, env = compile_expr(src, **locals())
        res = stmt()
        self.assertEqual(res, -111)
        self.assertEqual(accu1, [567])
        self.assertEqual(accu2, [-111])


    def test_try_exc_miss(self):
        accu1, bad_guy = make_raise_accumulator(BaseException)
        accu2, good_guy = make_accumulator()

        src = """
        (try
          (bad_guy 567)
          ((Exception) (good_guy -111)))
        """
        stmt, env = compile_expr(src, **locals())
        self.assertRaises(BaseException, stmt)
        self.assertEqual(accu1, [567])
        self.assertEqual(accu2, [])


    def test_keyword_try_exc_miss(self):
        accu1, bad_guy = make_raise_accumulator(BaseException)
        accu2, good_guy = make_accumulator()

        src = """
        (try
          (bad_guy 567)
          (except: Exception (good_guy -111)))
        """
        stmt, env = compile_expr(src, **locals())
        self.assertRaises(BaseException, stmt)
        self.assertEqual(accu1, [567])
        self.assertEqual(accu2, [])


    def test_named_try_exc_miss(self):
        accu1, bad_guy = make_raise_accumulator(BaseException)
        accu2, good_guy = make_accumulator()

        src = """
        (try
          (bad_guy 567)
          ((Exception as: e) (good_guy -111)))
        """
        stmt, env = compile_expr(src, **locals())
        self.assertRaises(BaseException, stmt)
        self.assertEqual(accu1, [567])
        self.assertEqual(accu2, [])


    def test_try_exc_else(self):

        accu1, bad_guy = make_raise_accumulator()
        accu2, good_guy = make_accumulator()

        src = """
        (try
          (bad_guy 567)
          ((Exception) (good_guy -111))
          (else: (good_guy 987)))
        """
        stmt, env = compile_expr(src, **locals())
        res = stmt()
        self.assertEqual(res, -111)
        self.assertEqual(accu1, [567])
        self.assertEqual(accu2, [-111])


    def test_keyword_try_exc_else(self):

        accu1, bad_guy = make_raise_accumulator()
        accu2, good_guy = make_accumulator()

        src = """
        (try
          (bad_guy 567)
          (except: Exception (good_guy -111))
          (else: (good_guy 987)))
        """
        stmt, env = compile_expr(src, **locals())
        res = stmt()
        self.assertEqual(res, -111)
        self.assertEqual(accu1, [567])
        self.assertEqual(accu2, [-111])


    def test_named_try_exc_else(self):

        accu1, bad_guy = make_raise_accumulator()
        accu2, good_guy = make_accumulator()

        src = """
        (try
          (bad_guy 567)
          ((Exception as: e) (good_guy -111))
          (else: (good_guy 987)))
        """
        stmt, env = compile_expr(src, **locals())
        res = stmt()
        self.assertEqual(res, -111)
        self.assertEqual(accu1, [567])
        self.assertEqual(accu2, [-111])


    def test_try_exc_finally(self):

        accu1, bad_guy = make_raise_accumulator()
        accu2, good_guy = make_accumulator()

        src = """
        (try
          (bad_guy 567)
          ((Exception) (good_guy -111))
          (finally: (good_guy 789)))
        """
        stmt, env = compile_expr(src, **locals())
        res = stmt()
        self.assertEqual(res, -111)
        self.assertEqual(accu1, [567])
        self.assertEqual(accu2, [-111, 789])


    def test_keyword_try_exc_finally(self):

        accu1, bad_guy = make_raise_accumulator()
        accu2, good_guy = make_accumulator()

        src = """
        (try
          (bad_guy 567)
          (except: Exception (good_guy -111))
          (finally: (good_guy 789)))
        """
        stmt, env = compile_expr(src, **locals())
        res = stmt()
        self.assertEqual(res, -111)
        self.assertEqual(accu1, [567])
        self.assertEqual(accu2, [-111, 789])


    def test_named_try_exc_finally(self):

        accu1, bad_guy = make_raise_accumulator()
        accu2, good_guy = make_accumulator()

        src = """
        (try
          (bad_guy 567)
          ((e Exception) (good_guy -111))
          (finally: (good_guy 789)))
        """
        stmt, env = compile_expr(src, **locals())
        res = stmt()
        self.assertEqual(res, -111)
        self.assertEqual(accu1, [567])
        self.assertEqual(accu2, [-111, 789])


    def test_try_exc_else_finally(self):

        accu1, bad_guy = make_raise_accumulator()
        accu2, good_guy = make_accumulator()

        src = """
        (try
          (bad_guy 567)
          ((Exception) (good_guy -111))
          (else: (good_guy 456))
          (finally: (good_guy 789)))
        """
        stmt, env = compile_expr(src, **locals())
        res = stmt()
        self.assertEqual(res, -111)
        self.assertEqual(accu1, [567])
        self.assertEqual(accu2, [-111, 789])


    def test_keyword_try_exc_else_finally(self):

        accu1, bad_guy = make_raise_accumulator()
        accu2, good_guy = make_accumulator()

        src = """
        (try
          (bad_guy 567)
          (except: Exception (good_guy -111))
          (else: (good_guy 456))
          (finally: (good_guy 789)))
        """
        stmt, env = compile_expr(src, **locals())
        res = stmt()
        self.assertEqual(res, -111)
        self.assertEqual(accu1, [567])
        self.assertEqual(accu2, [-111, 789])


    def test_named_try_exc_else_finally(self):

        accu1, bad_guy = make_raise_accumulator()
        accu2, good_guy = make_accumulator()

        src = """
        (try
          (bad_guy 567)
          ((Exception as: e) (good_guy -111))
          (else: (good_guy 456))
          (finally: (good_guy 789)))
        """
        stmt, env = compile_expr(src, **locals())
        res = stmt()
        self.assertEqual(res, -111)
        self.assertEqual(accu1, [567])
        self.assertEqual(accu2, [-111, 789])


    def test_hammer_try(self):

        accu1, bad_guy = make_raise_accumulator()
        accu2, good_guy = make_accumulator()
        counter = 5000

        src = """
        (while (< 0 counter)
          (setq counter (- counter 1))
          (try
            (bad_guy 567)
            ((Exception as: e) (good_guy -111))
            (else: (good_guy 456))
            (finally: (good_guy 789))))
        """
        stmt, env = compile_expr(src, **locals())
        res = stmt()
        self.assertEqual(res, -111)
        self.assertEqual(accu1, [567] * counter)
        self.assertEqual(accu2, [-111, 789] * counter)

        accu1, bad_guy = make_raise_accumulator()
        accu2, good_guy = make_accumulator()
        counter = 5000

        src = """
        (while (< 0 counter)
          (setq counter (- counter 1))
          (try
            (bad_guy 567)
            ((Exception) (good_guy -111))
            (else: (good_guy 456))
            (finally: (good_guy 789))))
        """
        stmt, env = compile_expr(src, **locals())
        res = stmt()
        self.assertEqual(res, -111)
        self.assertEqual(accu1, [567] * counter)
        self.assertEqual(accu2, [-111, 789] * counter)


class SpecialWith(TestCase):

    def test_simple_with(self):
        accu1, good_manager = make_manager()

        src = """
        (with (foo (good_manager 123 456 789))
          (foo 100))
        """
        stmt, env = compile_expr(src, **locals())
        res = stmt()
        self.assertEqual(res, 100)
        self.assertEqual(accu1, [123, 456, 100, 789])


    def test_raise_with(self):
        accu1, good_manager = make_manager()
        accu2, bad_guy = make_raise_accumulator()
        accu3, good_guy = make_accumulator()

        src = """
        (with (foo (good_manager 123 456 789))
          (foo 111)
          (foo (bad_guy (good_guy (foo 777)))))
        """
        stmt, env = compile_expr(src, **locals())
        self.assertRaises(Exception, stmt)
        self.assertEqual(accu1, [123, 456, 111, 777, 789])
        self.assertEqual(accu2, [777])
        self.assertEqual(accu3, [777])


class SpecialWhile(TestCase):

    def test_simple_while(self):
        accu1, good_guy = make_accumulator()

        src = """
        (while (< 0 x)
          (setq x (- x 1))
          (good_guy (+ 100 x)))
        """
        stmt, env = compile_expr(src, x=5, **locals())
        res = stmt()
        self.assertEqual(res, 100)
        self.assertEqual(accu1, [104, 103, 102, 101, 100])


    def test_errors(self):
        src = """
        (continue)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (while True (continue 1 2))
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (break)
        """
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (while True (break 1 2))
        """
        self.assertRaises(SyntaxError, compile_expr, src)


    def test_while_continue(self):
        accu1, good_guy = make_accumulator()

        src = """
        (while keep_going
          (setq x (- x 1))
          (cond
            [(< 0 x)
             (good_guy (+ 100 x))
             (continue)]
            [else:
             (setq keep_going False)
             654]))
        """
        stmt, env = compile_expr(src, keep_going=True, x=5, **locals())
        res = stmt()
        self.assertEqual(res, 654)
        self.assertEqual(accu1, [104, 103, 102, 101])

        accu1, good_guy = make_accumulator()

        src = """
        (while keep_going
          (setq x (- x 1))
          (cond
            [(< 0 x)
             (good_guy (+ 100 x))
             (continue 987)]
            [else:
             (setq keep_going False)
             654]))
        """
        stmt, env = compile_expr(src, keep_going=True, x=5, **locals())
        res = stmt()
        self.assertEqual(res, 654)
        self.assertEqual(accu1, [104, 103, 102, 101])


    def test_evil_continue(self):
        accu1, good_guy = make_accumulator()

        src = """
        (while keep_going
          (setq x (- x 1))
          (cond
            [(< 0 x)
             (good_guy (+ 100 x (continue)))]
            [else:
             (setq keep_going False)
             654]))
        """
        stmt, env = compile_expr(src, keep_going=True, x=500, **locals())
        res = stmt()
        self.assertEqual(res, 654)
        self.assertEqual(accu1, [])

        accu1, good_guy = make_accumulator()

        src = """
        (while keep_going
          (setq x (- x 1))
          (cond
            [(< 0 x)
             (good_guy (+ 100 x (continue 987)))]
            [else:
             (setq keep_going False)
             654]))
        """
        stmt, env = compile_expr(src, keep_going=True, x=500, **locals())
        res = stmt()
        self.assertEqual(res, 654)
        self.assertEqual(accu1, [])


    def test_while_break(self):
        accu1, good_guy = make_accumulator()

        src = """
        (while keep_going
          (setq x (- x 1))
          (cond
            [(< 0 x)
             (good_guy (+ 100 x))]
            [else:
             (setq keep_going False)
             (break 654)]))
        """
        stmt, env = compile_expr(src, keep_going=True, x=5, **locals())
        res = stmt()
        self.assertEqual(res, 654)
        self.assertEqual(accu1, [104, 103, 102, 101])

        accu1, good_guy = make_accumulator()

        src = """
        (while keep_going
          (setq x (- x 1))
          (cond
            [(< 0 x)
             (good_guy (+ 100 x))]
            [else:
             (setq keep_going False)
             (break)]))
        """
        stmt, env = compile_expr(src, keep_going=True, x=5, **locals())
        res = stmt()
        self.assertEqual(res, None)
        self.assertEqual(accu1, [104, 103, 102, 101])


    def test_evil_break(self):
        accu1, good_guy = make_accumulator()

        src = """
        (while keep_going
          (setq x (- x 1))
          (cond
            [(< 0 x)
             (good_guy (+ 100 x))]
            [else:
             (setq keep_going False)
             (good_guy (+ 100 x (break 654)))]))
        """
        stmt, env = compile_expr(src, keep_going=True, x=5, **locals())
        res = stmt()
        self.assertEqual(res, 654)
        self.assertEqual(accu1, [104, 103, 102, 101])

        accu1, good_guy = make_accumulator()

        src = """
        (while keep_going
          (setq x (- x 1))
          (cond
            [(< 0 x)
             (good_guy (+ 100 x))]
            [else:
             (setq keep_going False)
             (good_guy (+ 100 x (break)))]))
        """
        stmt, env = compile_expr(src, keep_going=True, x=5, **locals())
        res = stmt()
        self.assertEqual(res, None)
        self.assertEqual(accu1, [104, 103, 102, 101])


class SpecialSetqValues(TestCase):


    def test_simple_values(self):

        src = """
        (setq-values (a b c) (values 9 8 7))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(env["a"], 9)
        self.assertEqual(env["b"], 8)
        self.assertEqual(env["c"], 7)

        src = """
        (setq-values (a b . c) (values 9 8 7 6 5))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(env["a"], 9)
        self.assertEqual(env["b"], 8)
        self.assertEqual(env["c"], [7, 6, 5])


    def test_nested_values(self):

        src = """
        (setq-values (a (b c)) (values 1 (values 2 3)))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(env["a"], 1)
        self.assertEqual(env["b"], 2)
        self.assertEqual(env["c"], 3)


        src = """
        (setq-values (a (b c)) '(1 2 . 3))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(env["a"], 1)
        self.assertEqual(env["b"], 2)
        self.assertEqual(env["c"], 3)


    def test_star_values(self):

        src = """
        (setq-values (a b *: c) (values 1 2 3 4 5))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(env["a"], 1)
        self.assertEqual(env["b"], 2)
        self.assertEqual(env["c"], [3, 4, 5])

        src = """
        (setq-values (a b *: c) (values 1 2))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(env["a"], 1)
        self.assertEqual(env["b"], 2)
        self.assertEqual(env["c"], [])

        src = """
        (setq-values (a *: b c) (values 1 2 3 4 5))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(env["a"], 1)
        self.assertEqual(env["b"], [2, 3, 4])
        self.assertEqual(env["c"], 5)

        src = """
        (setq-values (*: a b c) (values 1 2 3 4 5))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(env["a"], [1, 2, 3])
        self.assertEqual(env["b"], 4)
        self.assertEqual(env["c"], 5)

        src = """
        (setq-values (*: a b c) (values 1 2))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(env["a"], [])
        self.assertEqual(env["b"], 1)
        self.assertEqual(env["c"], 2)

        src = """
        (setq-values
          (*: a (b c *: d) e)
          (values 1 2 3 (values 4 5 6 7) 8))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(env["a"], [1, 2, 3])
        self.assertEqual(env["b"], 4)
        self.assertEqual(env["c"], 5)
        self.assertEqual(env["d"], [6, 7])
        self.assertEqual(env["e"], 8)

        src = """
        (setq-values
          (*: a (b c . d) e)
          (values 1 2 3 (values 4 5 6 7) 8))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(env["a"], [1, 2, 3])
        self.assertEqual(env["b"], 4)
        self.assertEqual(env["c"], 5)
        self.assertEqual(env["d"], [6, 7])
        self.assertEqual(env["e"], 8)


class SpecialDefineValues(TestCase):


    def test_simple_values(self):

        src = """
        (define-values (a b c) (values 9 8 7))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(env["a"], 9)
        self.assertEqual(env["b"], 8)
        self.assertEqual(env["c"], 7)

        src = """
        (define-values (a b . c) (values 9 8 7 6 5))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(env["a"], 9)
        self.assertEqual(env["b"], 8)
        self.assertEqual(env["c"], [7, 6, 5])


    def test_nested_values(self):

        src = """
        (define-values (a (b c)) (values 1 (values 2 3)))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(env["a"], 1)
        self.assertEqual(env["b"], 2)
        self.assertEqual(env["c"], 3)


        src = """
        (define-values (a (b c)) '(1 2 . 3))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(env["a"], 1)
        self.assertEqual(env["b"], 2)
        self.assertEqual(env["c"], 3)


    def test_star_values(self):

        src = """
        (define-values (a b *: c) (values 1 2 3 4 5))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(env["a"], 1)
        self.assertEqual(env["b"], 2)
        self.assertEqual(env["c"], [3, 4, 5])

        src = """
        (define-values (a b *: c) (values 1 2))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(env["a"], 1)
        self.assertEqual(env["b"], 2)
        self.assertEqual(env["c"], [])

        src = """
        (define-values (a *: b c) (values 1 2 3 4 5))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(env["a"], 1)
        self.assertEqual(env["b"], [2, 3, 4])
        self.assertEqual(env["c"], 5)

        src = """
        (define-values (*: a b c) (values 1 2 3 4 5))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(env["a"], [1, 2, 3])
        self.assertEqual(env["b"], 4)
        self.assertEqual(env["c"], 5)

        src = """
        (define-values (*: a b c) (values 1 2))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(env["a"], [])
        self.assertEqual(env["b"], 1)
        self.assertEqual(env["c"], 2)

        src = """
        (define-values
          (*: a (b c *: d) e)
          (values 1 2 3 (values 4 5 6 7) 8))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(env["a"], [1, 2, 3])
        self.assertEqual(env["b"], 4)
        self.assertEqual(env["c"], 5)
        self.assertEqual(env["d"], [6, 7])
        self.assertEqual(env["e"], 8)

        src = """
        (define-values
          (*: a (b c . d) e)
          (values 1 2 3 (values 4 5 6 7) 8))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(env["a"], [1, 2, 3])
        self.assertEqual(env["b"], 4)
        self.assertEqual(env["c"], 5)
        self.assertEqual(env["d"], [6, 7])
        self.assertEqual(env["e"], 8)


class SpecialForEach(TestCase):

    def test_range(self):

        src = """
        (let ((z 0))
          (for-each (x (range 0 10))
            (setq z (+ x z))
             z))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, 45)


    def test_continue(self):

        data = []
        src = """
        (for-each [i (range 8)]
          (cond [(% i 2) (continue)] [else: (collect i)]))
        """
        stmt, env = compile_expr(src, collect=data.append)
        res = stmt()
        self.assertEqual(res, None)
        self.assertEqual(data, [0, 2, 4, 6])

        data = []
        src = """
        (for-each [i (range 8)]
          (cond [(% i 2) (continue i)] [else: (collect i)]))
        """
        stmt, env = compile_expr(src, collect=data.append)
        res = stmt()
        self.assertEqual(res, 7)
        self.assertEqual(data, [0, 2, 4, 6])


    def test_unpack_enum(self):

        accu1, good_guy = make_accumulator()

        src = """
        (let ((z 0))
          (for-each ((x y) (enumerate (range 0 5)))
             (good_guy (values x y z))
             (setq z (+ x y z))))
        """
        stmt, env = compile_expr(src, good_guy=good_guy)
        res = stmt()
        self.assertEqual(res, None)
        self.assertEqual(accu1, [(0, 0, 0),
                                 (1, 1, 0),
                                 (2, 2, 2),
                                 (3, 3, 6),
                                 (4, 4, 12)])


class SpecialYield(TestCase):

    def test_yield(self):
        src = """
        (let ((x 5) (y 0))
          (while x
            (yield (values x y))
            (setq y (+ y 1))
            (setq x (- x 1))))
        """
        stmt, env = compile_expr(src)
        res = stmt()

        self.assertEqual(type(res), GeneratorType)
        self.assertEqual(next(res), (5, 0))
        self.assertEqual(list(res), [(4, 1), (3, 2),
                                     (2, 3), (1, 4)])


class SpecialAwait(AsyncTestCase):

    def test_await(self):
        import math

        inputs = (4, 3, 2)

        src = """
        (begin
          (define asyncio (import asyncio))
          (define factorial
            (function factorial [number]
              (cond
               [(< number 0)
                (raise (ArgumentError "undefined for negative numbers"))]
               [(<= number 2) number]
               [else:
                (* number (await (factorial (- number 1))))])))
          (function main [*: inputs]
            (await (asyncio.gather *: (map factorial inputs)))))
        """
        stmt, env = compile_expr(src)
        awaitable = stmt()
        res = self.loop.run_until_complete(awaitable(*inputs))

        self.assertEqual(res, list(map(math.factorial, inputs)))


class SpecialDeclareAsync(AsyncTestCase):

    # test that the coroutine makes it through the asyncio event loop
    def test_declare_async(self):
        src = """
        (function coro-echo [obj]
          (declare-async)
          obj)
        """
        stmt, env = compile_expr(src)
        awaitable = stmt()
        input_atom = object()
        res = self.loop.run_until_complete(awaitable(input_atom))

        self.assertEqual(res, input_atom)

    # every expression has a value
    def test_declare_async_expr(self):
        src = """
        (function coro-echo [obj]
          obj
          (declare-async))
        """
        stmt, env = compile_expr(src)
        awaitable = stmt()
        input_atom = object()
        res = self.loop.run_until_complete(awaitable(input_atom))

        self.assertEqual(res, None)


def asyncio_run(awaitable):
    import sys, asyncio

    # before python 3.7 this is pretty awkward
    if sys.version_info.minor < 7:
        loop = asyncio.get_event_loop()
        res = loop.run_until_complete(awaitable)
        # normally, you would want to do this:
        #loop.close()
        # but, there doesn't seem to be a way to reset it
        return res
    else:
        return asyncio.run(awaitable)


class SpecialYieldFrom(TestCase):

    def test_yield_from(self):
        src = """
        (let ((x 5) (y 0))
          (yield (values None None))
          (yield-from
            (let ()
              (while x
                (yield (values x y))
                (setq y (+ y 1))
                (setq x (- x 1)))))
          (yield (values x y)))
        """
        stmt, env = compile_expr(src)
        res = stmt()

        self.assertEqual(type(res), GeneratorType)
        self.assertEqual(next(res), (None, None))

        self.assertEqual(list(res), [(5, 0), (4, 1), (3, 2),
                                     (2, 3), (1, 4), (0, 5)])


class SetqDelq(TestCase):

    def test_setq_delq_closure(self):
        src = """
        (let [[data 123]]
          (define-global set-data (function set-data [value]
             (setq data value)))
          (define-global del-data (function del-data []
             (delq data)))
          (define-global get-data (function get-data []
             data))
          data)
        """
        stmt, env = compile_expr(src)
        res = stmt()

        self.assertEqual(res, 123)

        setdata = env["set-data"]
        getdata = env["get-data"]
        deldata = env["del-data"]

        self.assertEqual(getdata(), 123)
        self.assertEqual(setdata(321), None)
        self.assertEqual(getdata(), 321)

        self.assertEqual(deldata(), None)
        self.assertRaises(NameError, getdata)

        self.assertEqual(setdata(789), None)
        self.assertEqual(getdata(), 789)
        self.assertEqual(deldata(), None)

        self.assertRaises(NameError, getdata)


    def test_setq_delq_global(self):
        src = """
        (let []
          (define-global set-data (function set-data [value]
             (setq data value)))
          (define-global del-data (function del-data []
             (delq data)))
          (define-global get-data (function get-data []
             data))
          data)
        """
        stmt, env = compile_expr(src, data=123)
        res = stmt()

        self.assertEqual(res, 123)

        setdata = env["set-data"]
        getdata = env["get-data"]
        deldata = env["del-data"]

        self.assertEqual(getdata(), 123)
        self.assertEqual(setdata(321), None)
        self.assertEqual(getdata(), 321)
        self.assertEqual(env.get("data", None), 321)

        self.assertEqual(deldata(), None)
        self.assertRaises(NameError, getdata)
        self.assertEqual(env.get("data", None), None)

        self.assertEqual(setdata(789), None)
        self.assertEqual(getdata(), 789)
        self.assertEqual(env.get("data", None), 789)

        self.assertEqual(deldata(), None)
        self.assertRaises(NameError, getdata)
        self.assertEqual(env.get("data", None), None)


class Attr(TestCase):


    def test_attr(self):

        src = """
        (let [[data (Object)]]
          (define-global set-data (function set-data [value]
             (set-attr data sample value)))
          (define-global del-data (function del-data []
             (del-attr data sample)))
          (define-global get-data (function get-data []
             (attr data sample)))
          data)
        """
        stmt, env = compile_expr(src, Object=Object)
        data = stmt()

        setdata = env["set-data"]
        getdata = env["get-data"]
        deldata = env["del-data"]

        self.assertFalse(hasattr(data, "sample"))
        self.assertRaises(AttributeError, getdata)

        self.assertEqual(setdata(123), None)
        self.assertTrue(hasattr(data, "sample"))
        self.assertEqual(getdata(), 123)
        self.assertEqual(data.sample, 123)

        self.assertEqual(deldata(), None)
        self.assertFalse(hasattr(data, "sample"))
        self.assertRaises(AttributeError, getdata)


class GlobalAccessors(TestCase):


    def test_setq_global(self):
        src = """
        (define-global tacos 100)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), None)
        self.assertEqual(env["tacos"], 100)

        src = """
        (let ((beer 999))
          (define-global tacos beer)
          (define-global beer 777)
          beer)
        """
        stmt, env = compile_expr(src, tacos=5)
        self.assertEqual(stmt(), 999)
        self.assertEqual(env["tacos"], 999)
        self.assertEqual(env["beer"], 777)

        src = """
        (setq-global tacos 100)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), None)
        self.assertEqual(env["tacos"], 100)

        src = """
        (let ((beer 999))
          (setq-global tacos beer)
          (setq-global beer 777)
          beer)
        """
        stmt, env = compile_expr(src, tacos=5)
        self.assertEqual(stmt(), 999)
        self.assertEqual(env["tacos"], 999)
        self.assertEqual(env["beer"], 777)


    def test_global(self):
        src = """
        (let ((tacos 100))
          (global tacos))
        """
        stmt, env = compile_expr(src, tacos=5)
        self.assertEqual(stmt(), 5)

        src = """
        (let ((tacos 100))
          (+ (global tacos) tacos))
        """
        stmt, env = compile_expr(src, tacos=5)
        self.assertEqual(stmt(), 105)

        src = """
        (let ((tacos 100))
          (define-global tacos 90)
          (+ (global tacos) tacos))
        """
        stmt, env = compile_expr(src, tacos=5)
        self.assertEqual(stmt(), 190)


    def test_delq_global(self):
        src = """
        (let ((tacos 100))
          (delq-global tacos)
          tacos)
        """
        stmt, env = compile_expr(src, tacos=5)
        self.assertEqual(stmt(), 100)
        self.assertTrue("tacos" not in env)


class Refq(TestCase):


    def test_refq(self):
        src = """
        (let []       ;; refq requires non-global scope, let provides such
          (define X)  ;; X is declared, but unassigned a value
          (values
             (refq X)                   ;; cell X
             (lambda [] X)              ;; getter closure
             (lambda [Y] (setq X Y))))  ;; setter closure
        """
        stmt, env = compile_expr(src)
        cell, getter, setter = stmt()

        # annoyingly, there's no CellType exposure
        self.assertEqual(type(cell).__name__, "cell")

        self.assertRaises(NameError, getter)
        self.assertRaises(ValueError, getderef, cell)

        self.assertEqual(setderef(cell, 123), None)
        self.assertEqual(getter(), 123)
        self.assertEqual(getderef(cell), 123)

        self.assertEqual(clearderef(cell), None)

        self.assertRaises(NameError, getter)
        self.assertRaises(ValueError, getderef, cell)

        self.assertEqual(setderef(cell, 456), None)
        self.assertEqual(getter(), 456)
        self.assertEqual(getderef(cell), 456)


    def test_deref(self):
        src = """
        (let []
          (define X 123)
          (values
             (refq X)
             (lambda [] X)
             (lambda [Y] (setq X Y))))
        """
        stmt, env = compile_expr(src)
        cell, getter, setter = stmt()

        src = """
        (deref cell)
        """
        stmt, env = compile_expr(src, cell=cell)
        self.assertEqual(stmt(), 123)

        src = """
        (set-deref cell 456)
        """
        stmt, env = compile_expr(src, cell=cell)
        self.assertEqual(stmt(), None)
        self.assertEqual(getter(), 456)

        src = """
        (del-deref cell)
        """
        stmt, env = compile_expr(src, cell=cell)
        self.assertEqual(stmt(), None)
        self.assertRaises(NameError, getter)
        self.assertRaises(ValueError, getderef, cell)


#
# The end.
