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

from .compiler import compile_expr


class Object(object):
    pass


class BuiltinsSetBang(TestCase):

    def test_setf_var(self):
        src = """
        (begin
          (setf o 101)
          o)
        """
        stmt, env = compile_expr(src, o=100)
        res = stmt()
        self.assertEqual(res, 101)

        src = """
        (begin
          (setf o tacos)
          o)
        """
        stmt, env = compile_expr(src, o=100, tacos=888)
        res = stmt()
        self.assertEqual(res, 888)

        src = """
        (let ((x o))
            (setf x tacos)
            x)
        """
        stmt, env = compile_expr(src, o=100, tacos=777)
        res = stmt()
        self.assertEqual(res, 777)

        src = """
        (begin
          (setf o (i tacos))
          o)
        """
        stmt, env = compile_expr(src, o=100, tacos=888, i=lambda x:x)
        res = stmt()
        self.assertEqual(res, 888)

        src = """
        (let ((x o))
          (setf x (i tacos))
          x)
        """
        stmt, env = compile_expr(src, o=100, tacos=777, i=lambda x:x)
        res = stmt()
        self.assertEqual(res, 777)


    def test_setf_car(self):

        src = """
        (begin
          (setf (car o) 9)
          o)
        """
        stmt, env = compile_expr(src, o=cons(1, 2))
        res = stmt()
        self.assertEqual(res, cons(9, 2))


    def test_setbang_cdr(self):

        src = """
        (begin
          (setf (cdr o) 9)
          o)
        """
        stmt, env = compile_expr(src, o=cons(1, 2))
        res = stmt()
        self.assertEqual(res, cons(1, 9))


    def test_setbang_item(self):

        src = """
        (begin
          (setf (item o 1) 9)
          o)
        """
        stmt, env = compile_expr(src, o=[1, 2, 3])
        res = stmt()
        self.assertEqual(res, [1, 9, 3])


    def test_setbang_setf(self):

        o = Object()
        src = """
        (begin
          (setf o.bar tacos)
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
          (setf o.foo.bar.z tacos)
          o)
        """
        stmt, env = compile_expr(src, o=o, tacos=9)
        res = stmt()
        self.assertEqual(res.foo.bar.z, 9)


    def test_setbang_global(self):

        src = """
        (let ((tacos 999))
          (setf (global tacos) 9)
          tacos)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 999)
        self.assertEqual(env["tacos"], 9)

        src = """
        (let ((tacos 999))
          (setf (global tacos) 9)
          tacos)
        """
        stmt, env = compile_expr(src, tacos=5)
        self.assertEqual(stmt(), 999)
        self.assertEqual(env["tacos"], 9)


class BuiltinsMacroExpand(TestCase):


    def test_macroexpand_1(self):
        pass


#
# The end.
