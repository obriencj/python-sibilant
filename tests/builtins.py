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


from functools import partial
from unittest import TestCase

import sibilant.builtins

from sibilant import car, cdr, cons, nil, symbol

from sibilant.compiler import compile_from_str


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


class BuiltinsTest(TestCase):

    def test_setbang_var(self):
        src = """
        (begin
          (set! o 101)
          o)
        """
        stmt, env = compile_expr(src, o=100)
        res = stmt()
        self.assertEqual(res, 101)

        src = """
        (begin
          (set! o tacos)
          o)
        """
        stmt, env = compile_expr(src, o=100, tacos=888)
        res = stmt()
        self.assertEqual(res, 888)

        src = """
        (let ((x o))
            (set! x tacos)
            x)
        """
        stmt, env = compile_expr(src, o=100, tacos=777)
        res = stmt()
        self.assertEqual(res, 777)

        src = """
        (begin
          (set! o (i tacos))
          o)
        """
        stmt, env = compile_expr(src, o=100, tacos=888, i=lambda x:x)
        res = stmt()
        self.assertEqual(res, 888)

        src = """
        (let ((x o))
          (set! x (i tacos))
          x)
        """
        stmt, env = compile_expr(src, o=100, tacos=777, i=lambda x:x)
        res = stmt()
        self.assertEqual(res, 777)


    def test_setbang_car(self):

        src = """
        (begin
          (set! (car o) 9)
          o)
        """
        stmt, env = compile_expr(src, o=cons(1, 2))
        res = stmt()
        self.assertEqual(res, cons(9, 2))


    def test_setbang_cdr(self):

        src = """
        (begin
          (set! (cdr o) 9)
          o)
        """
        stmt, env = compile_expr(src, o=cons(1, 2))
        res = stmt()
        self.assertEqual(res, cons(1, 9))


    def test_setbang_item(self):

        src = """
        (begin
          (set! (item o 1) 9)
          o)
        """
        stmt, env = compile_expr(src, o=[1, 2, 3])
        res = stmt()
        self.assertEqual(res, [1, 9, 3])


    def test_setbang_setf(self):

        o = Object()
        src = """
        (begin
          (set! o.bar tacos)
          o)
        """
        stmt, env = compile_expr(src, o=o, tacos=9)
        res = stmt()
        self.assertEqual(res.bar, 9)

        o = Object()
        o.foo = Object()
        o.foo.bar = Object()
        src = """
        (begin
          (set! o.foo.bar.z tacos)
          o)
        """
        stmt, env = compile_expr(src, o=o, tacos=9)
        res = stmt()
        self.assertEqual(res.foo.bar.z, 9)


    def test_setbang_global(self):

        src = """
        (let ((tacos 999))
          (set! (global tacos) 9)
          tacos)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 999)
        self.assertEqual(env["tacos"], 9)

        src = """
        (let ((tacos 999))
          (set! (global tacos) 9)
          tacos)
        """
        stmt, env = compile_expr(src, tacos=5)
        self.assertEqual(stmt(), 999)
        self.assertEqual(env["tacos"], 9)



    def test_macroexpand_1(self):
        pass


#
# The end.
