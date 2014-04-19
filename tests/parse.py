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
unittest for sibilant.parse

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


from io import StringIO
from unittest import TestCase
from sibilant.parse import *


def collect_emissions(source):
    return list(parse(StringIO(source)))


class TestParse(TestCase):

    def __init__(self, *args, **kwds):
        super(TestParse, self).__init__(*args, **kwds)
        self.maxDiff = None


    def test_symbol(self):
        src = "lambda"
        col = collect_emissions(src)
        exp = [(E_SYMBOL, (1, 0), "lambda")]

        self.assertEqual(col, exp)


    def test_number(self):
        src = "123"
        col = collect_emissions(src)
        exp = [(E_NUMBER, (1, 0), "123")]

        self.assertEqual(col, exp)


    def test_sharp(self):
        src = "#t"
        col = collect_emissions(src)
        exp = [(E_SYMBOL, (1, 0), "#t")]

        self.assertEqual(col, exp)


    def test_string(self):
        src = '"hello world"'
        col = collect_emissions(src)
        exp = [(E_STRING, (1, 0), "hello world")]

        self.assertEqual(col, exp)


    def test_quote_symbol(self):
        src = "'foo"
        col = collect_emissions(src)
        exp = [(E_QUOTE, (1, 0)),
               (E_SYMBOL, (1, 1), "foo")]

        self.assertEqual(col, exp)


    def test_quote_list(self):
        src = "'(foo bar)"
        col = collect_emissions(src)
        exp = [(E_QUOTE, (1, 0)),
               (E_OPEN, (1, 1)),
               (E_SYMBOL, (1, 2), "foo"),
               (E_SYMBOL, (1, 6), "bar"),
               (E_CLOSE, (1, 9))]

        self.assertEqual(col, exp)


    def test_quasi(self):
        src = "`bar"
        col = collect_emissions(src)
        exp = [(E_QUASI, (1, 0)),
               (E_SYMBOL, (1, 1), "bar")]

        self.assertEqual(col, exp)


    def test_unquote(self):
        src = ",baz"
        col = collect_emissions(src)
        exp = [(E_UNQUOTE, (1, 0)),
               (E_SYMBOL, (1, 1), "baz")]

        self.assertEqual(col, exp)


    def test_splice(self):
        src = "@qux"
        col = collect_emissions(src)
        exp = [(E_SPLICE, (1, 0)),
               (E_SYMBOL, (1, 1), "qux")]

        self.assertEqual(col, exp)


    def test_list(self):
        src = "(testing a thing)"
        col = collect_emissions(src)
        exp = [(E_OPEN, (1, 0)),
               (E_SYMBOL, (1, 1), "testing"),
               (E_SYMBOL, (1, 9), "a"),
               (E_SYMBOL, (1, 11), "thing"),
               (E_CLOSE, (1, 16))]

        self.assertEqual(col, exp)


    def test_dot(self):
        src = "(testing . 123)"
        col = collect_emissions(src)
        exp = [(E_OPEN, (1, 0)),
               (E_SYMBOL, (1, 1), "testing"),
               (E_DOT, (1, 9)),
               (E_NUMBER, (1, 11), "123"),
               (E_CLOSE, (1, 14))]

        self.assertEqual(col, exp)


    def test_newline(self):
        src = """
        ( this is
        a test )
        """
        col = collect_emissions(src)
        exp = [(E_NEWLINE, (1, 0)),
               (E_OPEN, (2, 8)),
               (E_SYMBOL, (2, 10), "this"),
               (E_SYMBOL, (2, 15), "is"),
               (E_NEWLINE, (2, 17)),
               (E_SYMBOL, (3, 8), "a"),
               (E_SYMBOL, (3, 10), "test"),
               (E_CLOSE, (3, 15)),
               (E_NEWLINE, (3, 16))]

        self.assertEqual(col, exp)


    def test_comments(self):
        src = """
        ; Let's check out the comments
        ( this is ; well it's something
        a test ) ; this ought to work
        """
        col = collect_emissions(src)
        exp = [(E_NEWLINE, (1, 0)),
               (E_COMMENT, (2, 8), "; Let's check out the comments"),
               (E_NEWLINE, (2, 38)),
               (E_OPEN, (3, 8)),
               (E_SYMBOL, (3, 10), "this"),
               (E_SYMBOL, (3, 15), "is"),
               (E_COMMENT, (3, 18), "; well it's something"),
               (E_NEWLINE, (3, 39)),
               (E_SYMBOL, (4, 8), "a"),
               (E_SYMBOL, (4, 10), "test"),
               (E_CLOSE, (4, 15)),
               (E_COMMENT, (4, 17), "; this ought to work"),
               (E_NEWLINE, (4, 37))]

        self.assertEqual(col, exp)


#
# The end.
