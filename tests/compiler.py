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

from sibilant import car, cdr, cons, nil, symbol, make_proper

from sibilant.compiler import (
    macro, is_macro, Macro,
    compile_from_str, compile_from_stream,
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
    code = compile_from_str(src_str, env)
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


    def test_quasiquote(self):
        src = """
        `1
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, 1)

        src = """
        `,1
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, 1)

        src = """
        `tacos
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, symbol("tacos"))

        src = """
        `()
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, nil)

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

        src = """
        `(,@foo)
        """
        stmt, env = compile_expr(src, foo=cons(1, 2, 3, nil))
        res = stmt()
        self.assertEqual(res, cons(1, 2, 3, nil))


    def test_nested_quasiquote(self):

        src = """
        `(1 2 `(foo ,(+ 1 2)))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        exp = cons(1, 2,
                   cons(symbol("quasiquote"),
                        cons(symbol("foo"),
                             cons(symbol("unquote"),
                                  cons(symbol("+"),
                                       1, 2,
                                       nil),
                                  nil),
                             nil),
                        nil),
                   nil)
        self.assertEqual(res, exp)

        src = """
        `(1 2 `(foo ,,(+ 1 2)))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        exp = cons(1, 2,
                   cons(symbol("quasiquote"),
                        cons(symbol("foo"),
                             cons(symbol("unquote"),
                                  3,
                                  nil),
                             nil),
                        nil),
                   nil)
        self.assertEqual(res, exp)

        src = """
        ``,(+ 1 2)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        exp = cons(symbol("quasiquote"),
                   cons(symbol("unquote"),
                        cons(symbol("+"),
                             1, 2,
                             nil),
                        nil),
                   nil)
        self.assertEqual(res, exp)

        src = """
        ``,,(+ 1 2)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        exp = cons(symbol("quasiquote"),
                   cons(symbol("unquote"),
                        3,
                        nil),
                   nil)
        self.assertEqual(res, exp)

        src = """
        `(1 `(bar ,@,foo))
        """
        stmt, env = compile_expr(src, foo=cons(1, 2, 3, nil))
        res = stmt()
        exp = cons(1,
                   cons(symbol("quasiquote"),
                        cons(symbol("bar"),
                             cons(symbol("unquote-splicing"),
                                  cons(1, 2, 3, nil),
                                  nil),
                             nil),
                        nil),
                   nil)
        self.assertEqual(res, exp)


    def test_getf(self):

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
        (getf o.foo.bar baz)
        """
        stmt, env = compile_expr(src, o=o)
        res = stmt()
        self.assertEqual(res, 111)


    def test_getf(self):

        o = Object()
        o.foo = Object()
        o.foo.bar = Object()
        o.foo.bar.baz = 111

        src = """
        (setf o.foo.bar baz 999)
        """
        stmt, env = compile_expr(src, o=o)
        res = stmt()
        self.assertEqual(res, None)
        self.assertEqual(o.foo.bar.baz, 999)

        src = """
        (setf (getf o.foo bar) baz 888)
        """
        stmt, env = compile_expr(src, o=o)
        res = stmt()
        self.assertEqual(res, None)
        self.assertEqual(o.foo.bar.baz, 888)


class CompilerClosures(TestCase):

    def test_4(self):
        src = """
        (lambda (a b)
          (set-var a ((lambda (x) (+ a x)) 9))
          (cons a b))
        """
        stmt, env = compile_expr(src)
        fun = stmt()
        res = fun(100, 200)
        self.assertTrue(res, cons(109, 200))

        src = """
        (lambda (a b)
          (set-var b ((lambda (x) (+ b x)) 9))
          (cons a b))
        """
        stmt, env = compile_expr(src)
        fun = stmt()
        res = fun(100, 200)
        self.assertTrue(res, cons(100, 209))

        src = """
        (lambda (a b)
          (set-var a ((lambda (x) (+ b x)) 9))
          (cons a b))
        """
        stmt, env = compile_expr(src)
        fun = stmt()
        res = fun(100, 200)
        self.assertTrue(res, cons(209, 200))

        src = """
        (lambda (a b)
          (set-var b ((lambda (x) (+ a x)) 9))
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


class CompilerSpecials(TestCase):


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


    def test_define_global(self):
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


    def test_get_global(self):
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


    def test_define_local(self):
        src = """
        (begin
          (define-local tacos 100)
          tacos)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 100)
        self.assertNotIn("tacos", env)

        src = """
        (begin
          (define-local tacos 100)
          tacos)
        """
        stmt, env = compile_expr(src, tacos=5)
        self.assertEqual(stmt(), 100)
        self.assertEqual(env["tacos"], 5)

        src = """
        (let ((beer 999))
          (define-local tacos beer)
          (define-local nachos 100)
          (+ nachos tacos))
        """
        stmt, env = compile_expr(src, tacos=5)
        self.assertEqual(stmt(), 1099)
        self.assertNotIn("beer", env)
        self.assertNotIn("nachos", env)


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
          (set! X (- X 1))
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
          (else (good_guy 888)))
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
          (finally (good_guy 999)))
        """
        stmt, env = compile_expr(src, good_guy=good_guy)
        res = stmt()
        self.assertEqual(res, 999)
        self.assertEqual(accu, [567, 999])


    def test_try_noexc_else_finally(self):

        accu, good_guy = make_accumulator()

        src = """
        (try
          (good_guy 567)
          (else (good_guy 888))
          (finally (good_guy 999)))
        """
        stmt, env = compile_expr(src, good_guy=good_guy)
        dis.dis(stmt)
        res = stmt()
        self.assertEqual(res, 999)
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
          (Exception (good_guy -111)))
        """
        stmt, env = compile_expr(src, **locals())
        res = stmt()
        self.assertEqual(res, -111)
        self.assertEqual(accu1, [567])
        self.assertEqual(accu2, [-111])


    def _test_try_exc_miss(self):
        accu1, bad_guy = make_raise_accumulator(BaseException)
        accu2, good_guy = make_accumulator()

        src = """
        (try
          (bad_guy 567)
          (Exception (good_guy -111)))
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
          (Exception (good_guy -111))
          (else (good_guy 987)))
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
          (Exception (good_guy -111))
          (finally (good_guy 789)))
        """
        stmt, env = compile_expr(src, **locals())
        res = stmt()
        self.assertEqual(res, 789)
        self.assertEqual(accu1, [567])
        self.assertEqual(accu2, [-111, 789])


    def test_try_exc_else_finally(self):

        accu1, bad_guy = make_raise_accumulator()
        accu2, good_guy = make_accumulator()

        src = """
        (try
          (bad_guy 567)
          (Exception (good_guy -111))
          (else (good_guy 456))
          (finally (good_guy 789)))
        """
        stmt, env = compile_expr(src, **locals())
        res = stmt()
        self.assertEqual(res, 789)
        self.assertEqual(accu1, [567])
        self.assertEqual(accu2, [-111, 789])


#
# The end.
