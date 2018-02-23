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
from io import StringIO
from types import CodeType
from unittest import TestCase

import sibilant.builtins

from sibilant import car, cdr, cons, nil, symbol

from sibilant.compiler import (
    is_macro, Macro, is_alias, Alias,
)

from . import compile_expr


class Object(object):
    pass


class BuiltinsSetf(TestCase):

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


    def test_setf_cdr(self):

        src = """
        (begin
          (setf (cdr o) 9)
          o)
        """
        stmt, env = compile_expr(src, o=cons(1, 2))
        res = stmt()
        self.assertEqual(res, cons(1, 9))


    def test_setf_item(self):

        src = """
        (begin
          (setf (item o 1) 9)
          o)
        """
        stmt, env = compile_expr(src, o=[1, 2, 3])
        res = stmt()
        self.assertEqual(res, [1, 9, 3])


    def test_setf_member(self):

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


    def test_setbang_item_slice(self):

        seq = [1, 2, 3, 4, 5, 6, 7, 8, 9]

        src = """
        (setf (item-slice seq 0 3) (values 123 456 789))
        """
        stmt, env = compile_expr(src, seq=seq)
        self.assertEqual(stmt(), None)
        self.assertEqual(seq, [123, 456, 789, 4, 5, 6, 7, 8, 9])

        seq = [1, 2, 3, 4, 5, 6, 7, 8, 9]

        src = """
        (setf (item-slice seq step: 3) (values 123 456 789))
        """
        stmt, env = compile_expr(src, seq=seq)
        self.assertEqual(stmt(), None)
        self.assertEqual(seq, [123, 2, 3, 456, 5, 6, 789, 8, 9])


class BuiltinsMacroExpansion(TestCase):

    def test_macro(self):
        src = """
        (let ()
          (defimport itertools)
          (var counter (itertools.count))

          (defmacro value++ () (next counter)))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, None)

        value_macro = env["value++"]

        self.assertTrue(is_macro(value_macro))

        src = """
        (values (value++) (value++) (value++))
        """
        stmt, env = compile_expr(src, **env)
        res = stmt()
        self.assertEqual(res, (0, 1, 2))


    def test_alias(self):
        src = """
        (let ()
          (defimport itertools)
          (var counter (itertools.count))

          (defalias value++ (next counter)))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, None)

        value_macro = env["value++"]

        self.assertTrue(is_macro(value_macro))
        self.assertTrue(is_alias(value_macro))

        src = """
        (values value++ value++ value++)
        """
        stmt, env = compile_expr(src, **env)
        res = stmt()
        self.assertEqual(res, (0, 1, 2))


class BuiltinsEval(TestCase):

    def test_eval_str(self):
        src = """
        (eval "(+ 1 2 tacos)")
        """
        stmt, env = compile_expr(src, tacos=5)
        self.assertEqual(stmt(), 8)


    def test_eval_stream(self):
        data = """
        (+ 1 2 tacos)
        """
        data = StringIO(data)

        src = """
        (eval source_stream)
        """
        stmt, env = compile_expr(src, source_stream=data, tacos=5)
        self.assertEqual(stmt(), 8)


    def test_eval_pair(self):
        data = cons(symbol("+"), 1, 2, symbol("tacos"), nil)

        src = """
        (eval source_obj)
        """
        stmt, env = compile_expr(src, source_obj=data, tacos=5)
        self.assertEqual(stmt(), 8)


    def test_eval_symbol(self):
        data = symbol("tacos")

        src = """
        (eval source_sym)
        """
        stmt, env = compile_expr(src, source_sym=data, tacos=5)
        self.assertEqual(stmt(), 5)


class BuiltinsCompile(TestCase):

    def test_compile_str(self):
        src = """
        (compile "(+ 1 2 tacos)")
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertIs(type(res), CodeType)
        self.assertEqual(eval(res, {"tacos": 5}), 8)


    def test_compile_stream(self):
        data = """
        (+ 1 2 tacos)
        """
        data = StringIO(data)

        src = """
        (compile source_stream)
        """
        stmt, env = compile_expr(src, source_stream=data)
        res = stmt()
        self.assertIs(type(res), CodeType)
        self.assertEqual(eval(res, {"tacos": 5}), 8)


    def test_compile_pair(self):
        data = cons(symbol("+"), 1, 2, symbol("tacos"), nil)

        src = """
        (compile source_obj)
        """
        stmt, env = compile_expr(src, source_obj=data, tacos=5)
        res = stmt()
        self.assertIs(type(res), CodeType)
        self.assertEqual(eval(res, {"tacos": 5}), 8)


    def test_compile_symbol(self):
        data = symbol("tacos")

        src = """
        (compile source_sym)
        """
        stmt, env = compile_expr(src, source_sym=data)
        res = stmt()
        self.assertIs(type(res), CodeType)
        self.assertEqual(eval(res, {"tacos": 5}), 5)



#
# The end.
