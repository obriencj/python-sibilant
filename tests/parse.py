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

from sibilant.parse import Event, parse


def collect_emissions(source):
    return list(parse(StringIO(source)))


class TestParse(TestCase):

    def __init__(self, *args, **kwds):
        super(TestParse, self).__init__(*args, **kwds)
        self.maxDiff = None


    def test_symbol(self):
        src = "lambda"
        col = collect_emissions(src)
        exp = [(Event.SYMBOL, (1, 0), "lambda")]

        self.assertEqual(col, exp)


    def test_number(self):
        src = "123"
        col = collect_emissions(src)
        exp = [(Event.NUMBER, (1, 0), "123")]

        self.assertEqual(col, exp)


    def test_sharp(self):
        src = "#t"
        col = collect_emissions(src)
        exp = [(Event.SYMBOL, (1, 0), "#t")]

        self.assertEqual(col, exp)


    def test_string(self):
        src = '"hello world"'
        col = collect_emissions(src)
        exp = [(Event.STRING, (1, 0), "hello world")]

        self.assertEqual(col, exp)


    def test_quote_symbol(self):
        src = "'foo"
        col = collect_emissions(src)
        exp = [(Event.QUOTE, (1, 0)),
               (Event.SYMBOL, (1, 1), "foo")]

        self.assertEqual(col, exp)


    def test_quote_list(self):
        src = "'(foo bar)"
        col = collect_emissions(src)
        exp = [(Event.QUOTE, (1, 0)),
               (Event.OPEN, (1, 1)),
               (Event.SYMBOL, (1, 2), "foo"),
               (Event.SYMBOL, (1, 6), "bar"),
               (Event.CLOSE, (1, 9))]

        self.assertEqual(col, exp)


    def test_quasi(self):
        src = "`bar"
        col = collect_emissions(src)
        exp = [(Event.QUASI, (1, 0)),
               (Event.SYMBOL, (1, 1), "bar")]

        self.assertEqual(col, exp)


    def test_unquote(self):
        src = ",baz"
        col = collect_emissions(src)
        exp = [(Event.UNQUOTE, (1, 0)),
               (Event.SYMBOL, (1, 1), "baz")]

        self.assertEqual(col, exp)


    def test_splice(self):
        src = "@qux"
        col = collect_emissions(src)
        exp = [(Event.SPLICE, (1, 0)),
               (Event.SYMBOL, (1, 1), "qux")]

        self.assertEqual(col, exp)


    def test_quote_unquote_splice(self):
        src = """`(,@foo)"""
        col = collect_emissions(src)
        exp = [(Event.QUASI, (1, 0)),
               (Event.OPEN, (1, 1)),
               (Event.UNQUOTE, (1, 2)),
               (Event.SPLICE, (1, 3)),
               (Event.SYMBOL, (1, 4), "foo"),
               (Event.CLOSE, (1, 7))]

        self.assertEqual(col, exp)


    def test_list(self):
        src = "(testing a thing)"
        col = collect_emissions(src)
        exp = [(Event.OPEN, (1, 0)),
               (Event.SYMBOL, (1, 1), "testing"),
               (Event.SYMBOL, (1, 9), "a"),
               (Event.SYMBOL, (1, 11), "thing"),
               (Event.CLOSE, (1, 16))]

        self.assertEqual(col, exp)


    def test_dot(self):
        src = "(testing . 123)"
        col = collect_emissions(src)
        exp = [(Event.OPEN, (1, 0)),
               (Event.SYMBOL, (1, 1), "testing"),
               (Event.DOT, (1, 9)),
               (Event.NUMBER, (1, 11), "123"),
               (Event.CLOSE, (1, 14))]

        self.assertEqual(col, exp)


    def test_newline(self):
        src = """
        ( this is
        a test )
        """
        col = collect_emissions(src)
        exp = [(Event.NEWLINE, (1, 0)),
               (Event.OPEN, (2, 8)),
               (Event.SYMBOL, (2, 10), "this"),
               (Event.SYMBOL, (2, 15), "is"),
               (Event.NEWLINE, (2, 17)),
               (Event.SYMBOL, (3, 8), "a"),
               (Event.SYMBOL, (3, 10), "test"),
               (Event.CLOSE, (3, 15)),
               (Event.NEWLINE, (3, 16))]

        self.assertEqual(col, exp)


    def test_comments(self):
        src = """
        ; Let's check out the comments
        ( this is ; well it's something
        a test ) ; this ought to work
        """
        col = collect_emissions(src)
        exp = [(Event.NEWLINE, (1, 0)),
               (Event.COMMENT, (2, 8), "; Let's check out the comments"),
               (Event.NEWLINE, (2, 38)),
               (Event.OPEN, (3, 8)),
               (Event.SYMBOL, (3, 10), "this"),
               (Event.SYMBOL, (3, 15), "is"),
               (Event.COMMENT, (3, 18), "; well it's something"),
               (Event.NEWLINE, (3, 39)),
               (Event.SYMBOL, (4, 8), "a"),
               (Event.SYMBOL, (4, 10), "test"),
               (Event.CLOSE, (4, 15)),
               (Event.COMMENT, (4, 17), "; this ought to work"),
               (Event.NEWLINE, (4, 37))]

        self.assertEqual(col, exp)


#
# The end.
