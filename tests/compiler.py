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


class CompilerClosures(TestCase):

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


    def test_named_try_exc(self):

        accu1, bad_guy = make_raise_accumulator()
        accu2, good_guy = make_accumulator()

        src = """
        (try
          (bad_guy 567)
          ((Exception e) (good_guy -111)))
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
          (Exception (good_guy -111)))
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
          ((Exception e) (good_guy -111)))
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


    def test_named_try_exc_else(self):

        accu1, bad_guy = make_raise_accumulator()
        accu2, good_guy = make_accumulator()

        src = """
        (try
          (bad_guy 567)
          ((Exception e) (good_guy -111))
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


    def test_named_try_exc_finally(self):

        accu1, bad_guy = make_raise_accumulator()
        accu2, good_guy = make_accumulator()

        src = """
        (try
          (bad_guy 567)
          ((Exception e) (good_guy -111))
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


    def test_named_try_exc_else_finally(self):

        accu1, bad_guy = make_raise_accumulator()
        accu2, good_guy = make_accumulator()

        src = """
        (try
          (bad_guy 567)
          ((Exception e) (good_guy -111))
          (else (good_guy 456))
          (finally (good_guy 789)))
        """
        stmt, env = compile_expr(src, **locals())
        res = stmt()
        self.assertEqual(res, 789)
        self.assertEqual(accu1, [567])
        self.assertEqual(accu2, [-111, 789])


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

        src = """
        (with (foo (good_manager 123 456 789))
          (foo (bad_guy 777)))
        """
        stmt, env = compile_expr(src, **locals())
        self.assertRaises(Exception, stmt)
        self.assertEqual(accu1, [123, 456, 789])
        self.assertEqual(accu2, [777])


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


    def test_power(self):

        src = "(operator? **)"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (** 2 2)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 4)

        src = """
        (** 4 1/2)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 2)

        src = """
        (** 4 0.5)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 2)


    def test_modulo(self):

        src = "(operator? %)"
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (% 5 2)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 1)

        src = """
        (% "first %s second %s third %s" (make-tuple 3 2 1))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), "first 3 second 2 third 1")



class UnaryOperators(TestCase):

    def test_not(self):
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
        (not (make-list 0))
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
        (not (make-list))
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
        (apply not `(,(make-list 0))))
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
        (apply not `(,(make-list)))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)


    def test_invert(self):
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
        (in X 1)
        """
        stmt, env = compile_expr(src, X=[0, 1, 2])
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (in X 9)
        """
        stmt, env = compile_expr(src, X=[0, 1, 2])
        res = stmt()
        self.assertEqual(res, False)


    def test_apply_in(self):
        src = """
        (apply in `(,X 1))
        """
        stmt, env = compile_expr(src, X=[0, 1, 2])
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (apply in `(,X 9))
        """
        stmt, env = compile_expr(src, X=[0, 1, 2])
        res = stmt()
        self.assertEqual(res, False)


    def test_not_in(self):
        src = """
        (not-in X 1)
        """
        stmt, env = compile_expr(src, X=[0, 1, 2])
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (not-in X 9)
        """
        stmt, env = compile_expr(src, X=[0, 1, 2])
        res = stmt()
        self.assertEqual(res, True)


    def test_apply_not_in(self):
        src = """
        (apply not-in `(,X 1))
        """
        stmt, env = compile_expr(src, X=[0, 1, 2])
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (apply not-in `(,X 9))
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


    def test_lte(self):
        src = """
        (<= 1 2)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (lte 1 2)
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
        (lte 99 99)
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
        (lte 2 1)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)


    def test_apply_lte(self):
        src = """
        (apply <= '(1 2))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (apply lte '(1 2))
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
        (apply lte '(99 99))
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
        (apply lte '(2 1))
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


    def test_gte(self):
        src = """
        (>= 1 2)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (gte 1 2)
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
        (gte 99 99)
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
        (gte 2 1)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)


    def test_apply_gte(self):
        src = """
        (apply >= '(1 2))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, False)

        src = """
        (apply gte '(1 2))
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
        (apply gte '(99 99))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, True)

        src = """
        (apply >= '(2 1))
        """
        stmt, env = compile_expr(src)
        res = stmt()


#
# The end.
