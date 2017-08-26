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
from functools import partial
from sys import getrecursionlimit, setrecursionlimit
from unittest import TestCase

import sibilant.builtins

from sibilant.compiler import iter_compile
from sibilant.compiler.tco import trampoline, tailcall


@contextmanager
def recursionlimit(limit=(getrecursionlimit() // 2)):
    original = getrecursionlimit()
    yield setrecursionlimit(limit)
    setrecursionlimit(original)
    assert getrecursionlimit() == original, "could not reset recursion limit"


def basic_env(**base):
    env = {"__builtins__": sibilant.builtins}
    env.update(base)
    return env


def compile_expr(src_str, **base):
    env = basic_env(**base)
    icode = iter_compile(src_str, env)
    code = next(icode)
    return partial(eval, code, env), env


@trampoline
def tco_factorial(num, accu=1):
    if num <= 1:
        return accu
    else:
        return tailcall(tco_factorial)(num - 1, accu * num)


def factorial(num, accu=1):
    if num <= 1:
        return accu
    else:
        return factorial(num - 1, accu * num)


@trampoline
def tco_fibonacci(num, accu=0):
    if num < 1:
        return accu
    else:
        return tailcall(tco_fibonacci)(num - 1, accu + num)


def fibonacci(num, accu=0):
    if num < 1:
        return accu
    else:
        return fibonacci(num - 1, accu + num)


@trampoline
def tco_even(num):
    return True if num == 0 else tailcall(tco_odd)(num - 1)


@trampoline
def tco_odd(num):
    return True if num == 1 else tailcall(tco_even)(num - 1)


def even(num):
    return True if num == 0 else odd(num - 1)


def odd(num):
    return True if num == 1 else even(num - 1)


class TestTailcall(TestCase):

    def test_factorial(self):
        count = getrecursionlimit()

        self.assertEqual(factorial(10), 3628800)

        self.assertRaises(RecursionError, factorial, count)
        self.assertTrue(tco_factorial, count)
        self.assertTrue(tco_factorial, count * 10)

        with recursionlimit(200):
            self.assertRaises(RecursionError, factorial, 400)
            self.assertTrue(tco_factorial, 400)
            self.assertTrue(tco_factorial, count * 10)

        for i in range(100):
            self.assertEqual(factorial(i), tco_factorial(i))


    def test_fibonacci(self):
        count = getrecursionlimit()

        self.assertEqual(fibonacci(10), 55)

        self.assertRaises(RecursionError, fibonacci, count)
        self.assertTrue(tco_fibonacci, count)
        self.assertTrue(tco_fibonacci, count * 10)

        with recursionlimit(200):
            self.assertRaises(RecursionError, fibonacci, 400)
            self.assertTrue(tco_fibonacci, 400)
            self.assertTrue(tco_fibonacci, count * 10)

        for i in range(100):
            self.assertEqual(fibonacci(i), tco_fibonacci(i))


    def test_even_odd(self):
        count = getrecursionlimit()

        self.assertTrue(even(8))
        self.assertTrue(odd(13))

        num = count * 2
        self.assertRaises(RecursionError, even, num)
        self.assertRaises(RecursionError, odd, num - 1)

        self.assertTrue(tco_even(num))
        self.assertTrue(tco_odd(num - 1))


class TestTCOCompiler(TestCase):

    def test_factorial(self):
        count = getrecursionlimit()

        src = """
        (function factorial (num :accu 1)
          (if (<= num 1) accu (factorial (- num 1) (* num accu))))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertTrue(callable(res))

        self.assertEqual(res(10), 3628800)
        self.assertTrue(res(count * 10))

        for i in range(100):
            self.assertEqual(factorial(i), res(i))

        with recursionlimit(200):
            self.assertTrue(res(400))
            self.assertTrue(res(count * 10))


    def test_fibonacci(self):
        count = getrecursionlimit()

        src = """
        (function fibonacci (num :accu 0)
          (if (< num 1) accu (fibonacci (- num 1) (+ num accu))))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertTrue(callable(res))

        self.assertEqual(res(10), 55)
        self.assertTrue(res(count * 10))

        for i in range(100):
            self.assertEqual(fibonacci(i), res(i))

        with recursionlimit(200):
            self.assertTrue(res(400))
            self.assertTrue(res(count * 10))


    def test_even_odd(self):
        src = """
        (begin
          (defun even (val)
             (if (== val 2) True (odd (- val 1))))
          (defun odd (val)
             (if (== val 1) True (even (- val 1))))
          (values even odd))
        """
        stmt, env = compile_expr(src)
        even, odd = stmt()

        self.assertTrue(even(8))
        self.assertTrue(odd(13))

        count = getrecursionlimit() * 2

        self.assertTrue(even(count))
        self.assertTrue(odd(count - 1))


#
# The end.