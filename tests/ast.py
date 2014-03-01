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
unittest for sibilant.ast

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


from unittest import TestCase
from sibilant.ast import *
from sibilant.parse import parse


def compose_ast(src_str, starting_line=1):
    tree = compose_from_str(src_str, starting_line=starting_line)
    return tree.translate()


class TestParse(TestCase):

    def test_symbol(self):
        src = "lambda"
        col = compose_ast(src)
        exp = Symbol(1, "lambda")

        self.assertEqual(col, exp)


    def test_number(self):
        src = "123"
        col = compose_ast(src)
        exp = Number(1, "123")

        self.assertEqual(col, exp)


    def test_bool(self):
        src = "#t"
        col = compose_ast(src)
        exp = Boolean(1, "t")

        self.assertEqual(col, exp)

        src = "#f"
        col = compose_ast(src)
        exp = Boolean(1, "f")

        self.assertEqual(col, exp)


    def test_string(self):
        src = '"hello world"'
        col = compose_ast(src)
        exp = String(1, "hello world")

        self.assertEqual(col, exp)


    def test_quote_symbol(self):
        src = "'foo"
        col = compose_ast(src)
        exp = Quote(1, Symbol(1, "foo"))

        self.assertEqual(col, exp)


    def test_quasi(self):
        src = "`bar"
        col = compose_ast(src)
        exp = Quasi(1, Symbol(1, "bar"))

        self.assertEqual(col, exp)


    def test_unquote(self):
        src = ",baz"
        col = compose_ast(src)
        exp = Unquote(1, Symbol(1, "baz"))

        self.assertEqual(col, exp)


    def test_splice(self):
        src = "@qux"
        col = compose_ast(src)
        exp = Splice(1, Symbol(1, "qux"))

        self.assertEqual(col, exp)


    def test_quote_list(self):
        src = "'(testing a thing)"
        col = compose_ast(src)
        exp = Quote(1,
                    List(1,
                         Symbol(1, "testing"),
                         Symbol(1, "a"),
                         Symbol(1, "thing")))

        self.assertEqual(col, exp)
        self.assertTrue(col.expression.proper)


    def test_quote_dot(self):
        src = "'(testing . 123)"
        col = compose_ast(src)

        exp = Quote(1,
                    List(1,
                         Symbol(1, "testing"),
                         Number(1, "123")))
        exp.expression.proper = False

        self.assertEqual(col, exp)
        self.assertFalse(col.expression.proper)


    def test_special_apply(self):
        src = "(testing for fun)"
        col = compose_ast(src)
        exp = Apply(1,
                    Symbol(1, "testing"),
                    Symbol(1, "for"),
                    Symbol(1, "fun"))

        self.assertEqual(col, exp)


    def test_special_begin(self):
        src = "(begin to dance)"
        col = compose_ast(src)
        exp = Begin(1,
                    Symbol(1, "to"),
                    Symbol(1, "dance"))

        self.assertEqual(col, exp)

        src = "(begin (to dance))"
        col = compose_ast(src)
        exp = Begin(1,
                    Apply(1,
                          Symbol(1, "to"),
                          Symbol(1, "dance")))

        self.assertEqual(col, exp)


#
# The end.
