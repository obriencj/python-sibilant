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

from contextlib import contextmanager
from fractions import Fraction as fraction
from functools import partial
from unittest import TestCase

from sibilant import (
    car, cdr, cons, nil,
    symbol, keyword, build_proper,
)

from sibilant.compiler import (
    is_macro, Macro,
    is_special, Special,
    CodeFlag,
)

from sibilant.module import (
    fake_module_from_env, init_module, load_module_1,
    run_time,
)

from sibilant.parse import source_str


class Object(object):
    pass


def compile_expr(src_str, **base):
    mod = fake_module_from_env(base)
    init_module(mod, source_str(src_str, "<unittest>"), None)

    partial_run_time = partial(partial, run_time)

    result = load_module_1(mod, run_time=partial_run_time)

    return result, mod.__dict__


def compile_expr_no_tco(src_str, **base):
    mod = fake_module_from_env(base)

    params = {"tco_enabled": False}

    init_module(mod, source_str(src_str, "<unittest>"), None,
                compiler_factory_params=params)

    partial_run_time = partial(partial, run_time)

    result = load_module_1(mod, run_time=partial_run_time)

    return result, mod.__dict__


def compile_dis_expr(src_str, **base):
    mod = fake_module_from_env(base)
    init_module(mod, source_str(src_str, "<unittest>"), None)

    code_objs = []
    def partial_run_time(module, code_obj):

        dis.show_code(code_obj)
        print("Disassembly:")
        dis.dis(code_obj)

        code_objs.append(code_obj)
        return partial(run_time, module, code_obj)

    result = load_module_1(mod, run_time=partial_run_time)

    return result, mod.__dict__


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

        src = "(build-list True 1 True 1 False 0 False 0)"
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


class KeywordArgs(TestCase):

    def _test_gather_formals(self):
        # todo: test calling gather_formals directly
        pass


    def _test_gather_parameters(self):
        # todo: test calling gather_parameters directly
        pass


    def test_macro_formals(self):
        src = """
        (defmacro test (work for: '_ in: () when: True unless: True)
          `(values work for in when unless))

        (test (+ a 5) a in seq))
        """
        stmt, env = compile_expr(src, seq=(1, 2, 3))
        res = stmt()
        # TODO


    def test_formals(self):

        compile_expr = compile_expr_no_tco

        src = """
        (lambda (a b c)
          (values a b c))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        code = res.__code__
        self.assertTrue(callable(res))
        self.assertEqual(code.co_argcount, 3)
        self.assertEqual(code.co_varnames, ('a', 'b', 'c'))
        self.assertFalse(code.co_flags & CodeFlag.VARARGS.value)
        self.assertFalse(code.co_flags & CodeFlag.VARKEYWORDS.value)
        self.assertEqual(res(1, 2, 3), (1, 2, 3))
        self.assertEqual(res(c=3, b=2, a=1), (1, 2, 3))
        self.assertRaises(TypeError, res, 1, 2, 3, 4)
        self.assertRaises(TypeError, res, 1)

        src = """
        (lambda (a b: 0 c: 1)
          (values a b c))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        code = res.__code__
        self.assertTrue(callable(res))
        self.assertEqual(code.co_argcount, 3)
        self.assertEqual(code.co_varnames, ('a', 'b', 'c'))
        self.assertEqual(res.__defaults__, (0, 1))
        self.assertFalse(code.co_flags & CodeFlag.VARARGS.value)
        self.assertFalse(code.co_flags & CodeFlag.VARKEYWORDS.value)
        self.assertEqual(res(1), (1, 0, 1))
        self.assertEqual(res(1, 2), (1, 2, 1))
        self.assertEqual(res(1, 2, 3), (1, 2, 3))
        self.assertEqual(res(9, b=8, c=7), (9, 8, 7))
        self.assertEqual(res(9, c=7, b=8), (9, 8, 7))
        self.assertEqual(res(a=9, c=7, b=8), (9, 8, 7))
        self.assertEqual(res(c=7, b=8, a=9), (9, 8, 7))
        self.assertRaises(TypeError, res, 1, 2, 3, 4)
        self.assertRaises(TypeError, res)

        src = """
        (lambda (a *: rest)
          (values a rest))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        code = res.__code__
        self.assertTrue(callable(res))
        self.assertEqual(code.co_argcount, 1)
        self.assertEqual(code.co_varnames, ('a', 'rest'))
        self.assertTrue(code.co_flags & CodeFlag.VARARGS.value)
        self.assertFalse(code.co_flags & CodeFlag.VARKEYWORDS.value)
        self.assertEqual(res(1), (1, ()))
        self.assertEqual(res(1, 2), (1, (2,)))
        self.assertEqual(res(1, 2, 3), (1, (2, 3)))
        self.assertEqual(res(1, 2, 3, 4), (1, (2, 3, 4)))
        self.assertRaises(TypeError, res, a=1, b=2)
        self.assertRaises(TypeError, res)

        src = """
        (lambda (a . rest)
          (values a rest))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        code = res.__code__
        self.assertTrue(callable(res))
        self.assertEqual(code.co_argcount, 1)
        self.assertEqual(code.co_varnames, ('a', 'rest'))
        self.assertTrue(code.co_flags & CodeFlag.VARARGS.value)
        self.assertFalse(code.co_flags & CodeFlag.VARKEYWORDS.value)
        self.assertEqual(res(1), (1, ()))
        self.assertEqual(res(1, 2), (1, (2, )))
        self.assertEqual(res(1, 2, 3), (1, (2, 3)))
        self.assertEqual(res(1, 2, 3, 4), (1, (2, 3, 4)))
        self.assertRaises(TypeError, res, a=1, b=2)
        self.assertRaises(TypeError, res)

        src = """
        (lambda rest
          rest)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        code = res.__code__
        self.assertTrue(callable(res))
        self.assertEqual(code.co_argcount, 0)
        self.assertEqual(code.co_varnames, ('rest',))
        self.assertTrue(code.co_flags & CodeFlag.VARARGS.value)
        self.assertFalse(code.co_flags & CodeFlag.VARKEYWORDS.value)
        self.assertEqual(res(), ())
        self.assertEqual(res(1), (1, ))
        self.assertEqual(res(1, 2), (1, 2))
        self.assertEqual(res(1, 2, 3), (1, 2, 3))
        self.assertRaises(TypeError, res, a=1, b=2)

        src = """
        (lambda (*: rest)
          rest)
        """
        stmt, env = compile_expr(src)
        res = stmt()
        code = res.__code__
        self.assertTrue(callable(res))
        self.assertEqual(code.co_argcount, 0)
        self.assertEqual(code.co_varnames, ('rest',))
        self.assertTrue(code.co_flags & CodeFlag.VARARGS.value)
        self.assertFalse(code.co_flags & CodeFlag.VARKEYWORDS.value)
        self.assertEqual(res(), ())
        self.assertEqual(res(1), (1,))
        self.assertEqual(res(1, 2), (1, 2))
        self.assertEqual(res(1, 2, 3), (1, 2, 3))
        self.assertRaises(TypeError, res, a=1, b=2)

        src = """
        (lambda (a **: rest)
          (values a rest))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        code = res.__code__
        self.assertTrue(callable(res))
        self.assertEqual(code.co_argcount, 1)
        self.assertEqual(code.co_varnames, ('a', 'rest'))
        self.assertFalse(code.co_flags & CodeFlag.VARARGS.value)
        self.assertTrue(code.co_flags & CodeFlag.VARKEYWORDS.value)
        self.assertEqual(res(a=1, b=2, c=3), (1, dict(b=2, c=3)))
        self.assertRaises(TypeError, res, 1, 2, 3)

        src = """
        (lambda (a: 0 *: rest)
          (values a rest))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        code = res.__code__
        self.assertTrue(callable(res))
        self.assertEqual(code.co_argcount, 1)
        self.assertEqual(code.co_varnames, ('a', 'rest'))
        self.assertEqual(res.__defaults__, (0,))
        self.assertTrue(code.co_flags & CodeFlag.VARARGS.value)
        self.assertFalse(code.co_flags & CodeFlag.VARKEYWORDS.value)
        self.assertEqual(res(), (0, ()))
        self.assertEqual(res(1), (1, ()))
        self.assertEqual(res(1, 2, 3), (1, (2, 3)))
        self.assertRaises(TypeError, res, 1, 2, 3, a=9)

        src = """
        (lambda (a: 0 . rest)
          (values a rest))
        """
        # don't mix keywords and improper varargs, it's too weird.
        self.assertRaises(SyntaxError, compile_expr, src)

        src = """
        (lambda (a: 0 **: rest)
          (values a rest))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        code = res.__code__
        self.assertTrue(callable(res))
        self.assertEqual(code.co_argcount, 1)
        self.assertEqual(code.co_varnames, ('a', 'rest'))
        self.assertEqual(res.__defaults__, (0,))
        self.assertFalse(code.co_flags & CodeFlag.VARARGS.value)
        self.assertTrue(code.co_flags & CodeFlag.VARKEYWORDS.value)
        self.assertEqual(res(b=2, c=3), (0, dict(b=2, c=3)))
        self.assertEqual(res(1, b=2, c=3), (1, dict(b=2, c=3)))
        self.assertEqual(res(a=1, b=2, c=3), (1, dict(b=2, c=3)))
        self.assertRaises(TypeError, res, 1, 2, 3)


    def test_parameters(self):
        def tst(a, b, c):
            return (a, b, c)

        src = """
        (lambda (self)
          (self.assertEqual (tst 1 2 3) (values 1 2 3))
          (self.assertEqual (tst c: 3 b: 2 a: 1) (values 1 2 3))
          (self.assertRaises TypeError tst 1 2 3 4)
          (self.assertRaises TypeError tst 1))
        """
        stmt, env = compile_expr(src, tst=tst)
        stmt()(self)

        def tst(a, b=0, c=0):
            return (a, b, c)

        src = """
        (lambda (self)
          (self.assertEqual (tst 1) (values 1 0 0))
          (self.assertEqual (tst 1 2) (values 1 2 0))
          (self.assertEqual (tst 1 2 3) (values 1 2 3))
          (self.assertEqual (tst 9 b: 8 c: 7) (values 9 8 7))
          (self.assertEqual (tst 9 c: 7 b: 8) (values 9 8 7))
          (self.assertEqual (tst a: 9 c: 7 b: 8) (values 9 8 7))
          (self.assertEqual (tst c: 7 b: 8 a: 9) (values 9 8 7))
          (self.assertRaises TypeError tst 1 2 3 4)
          (self.assertRaises TypeError tst))
        """
        stmt, env = compile_expr(src, tst=tst)
        stmt()(self)

        def tst(a, *rest):
            return (a, rest)

        src = """
        (lambda (self)
          (self.assertEqual (tst 1) (values 1 (tuple)))
          (self.assertEqual (tst 1 2) (values 1 (values 2)))
          (self.assertEqual (tst 1 2 3) (values 1 (values 2 3)))
          (self.assertEqual (tst 1 2 3 4) (values 1 (values 2 3 4)))
          (self.assertRaises TypeError tst a: 1 b: 2)
          (self.assertRaises TypeError tst))
        """
        stmt, env = compile_expr(src, tst=tst)
        stmt()(self)

        def tst(a, **rest):
            return (a, rest)

        src = """
        (lambda (self)
          (self.assertEqual (tst 1 b: 2 c: 3) (values 1 (dict b: 2 c: 3)))
          (self.assertEqual (tst a: 1 b: 2 c: 3)
                            (values 1 (dict b: 2 c: 3)))
          (self.assertRaises TypeError tst 1 2 3))
        """
        stmt, env = compile_expr(src, tst=tst)
        stmt()(self)

        def tst(a=0, *rest):
            return (a, rest)

        src = """
        (lambda (self)
          (self.assertEqual (tst) (values 0 (tuple)))
          (self.assertEqual (tst 1) (values 1 (tuple)))
          (self.assertEqual (tst 1 2 3) (values 1 (values 2 3)))
          (self.assertRaises TypeError tst 1 2 3 a: 9))
        """
        stmt, env = compile_expr(src, tst=tst)
        stmt()(self)

        def tst(a=0, **rest):
            return (a, rest)

        src = """
        (lambda (self)
          (self.assertEqual (tst b: 2 c: 3)
                            (values 0 (dict b: 2 c: 3)))
          (self.assertEqual (tst 1 b: 2 c: 3)
                            (values 1 (dict b: 2 c: 3)))
          (self.assertEqual (tst a: 1 b: 2 c: 3)
                            (values 1 (dict b: 2 c: 3)))
          (self.assertRaises TypeError tst 1 2 3))
        """
        stmt, env = compile_expr(src, tst=tst)
        stmt()(self)


#
# The end.
