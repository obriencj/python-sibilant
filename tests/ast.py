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


class TestParse(TestCase):

    def test_symbol(self):
        src = "lambda"
        col = compose_from_str(src)
        exp = Symbol((1, 0), "lambda")

        self.assertEqual(col, exp)


    def test_number(self):
        src = "123"
        col = compose_from_str(src)
        exp = Integer((1, 0), "123")

        self.assertEqual(col, exp)

        src = "1/2"
        col = compose_from_str(src)
        exp = Fraction((1, 0), "1/2")

        self.assertEqual(col, exp)
        src = "1.5"
        col = compose_from_str(src)
        exp = Decimal((1, 0), "1.5")

        self.assertEqual(col, exp)

        src = "8+1j"
        col = compose_from_str(src)
        exp = Complex((1, 0), "8+1j")

        self.assertEqual(col, exp)


    def test_string(self):
        src = '"hello world"'
        col = compose_from_str(src)
        exp = String((1, 0), "hello world")

        self.assertEqual(col, exp)


    def test_quote_symbol(self):
        src = "'foo"
        col = compose_from_str(src)
        exp = Quote((1, 0), Symbol((1, 1), "foo"))

        self.assertEqual(col, exp)


    def test_quasi(self):
        src = "`bar"
        col = compose_from_str(src)
        exp = Quasi((1, 0), Symbol((1, 1), "bar"))

        self.assertEqual(col, exp)


    def test_unquote(self):
        src = ",baz"
        col = compose_from_str(src)
        exp = Unquote((1, 0), Symbol((1, 1), "baz"))

        self.assertEqual(col, exp)


    def test_splice(self):
        src = "@qux"
        col = compose_from_str(src)
        exp = Splice((1, 0), Symbol((1, 1), "qux"))

        self.assertEqual(col, exp)


    def test_quote_list(self):
        src = "'(testing a thing)"
        col = compose_from_str(src)
        exp = Quote((1, 0),
                    List((1, 1),
                         Symbol((1, 2), "testing"),
                         Symbol((1, 10), "a"),
                         Symbol((1, 12), "thing")))

        self.assertEqual(col, exp)
        self.assertTrue(col.expression.proper)


    def test_quote_dot(self):
        src = "'(testing . 123)"
        col = compose_from_str(src)

        exp = Quote((1, 0),
                    List((1, 1),
                         Symbol((1, 2), "testing"),
                         Integer((1, 12), "123")))
        exp.expression.proper = False

        self.assertEqual(col, exp)
        self.assertFalse(col.expression.proper)


#
# The end.
