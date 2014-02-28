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


    def test_symbol(self):
        src = "lambda"
        col = collect_emissions(src)
        exp = [(E_SYMBOL, "lambda")]

        self.assertEqual(col, exp)


    def test_number(self):
        src = "123"
        col = collect_emissions(src)
        exp = [(E_NUMBER, "123")]

        self.assertEqual(col, exp)


    def test_sharp(self):
        src = "#t"
        col = collect_emissions(src)
        exp = [(E_SHARP,),
               (E_SYMBOL, "t")]

        self.assertEqual(col, exp)


    def test_string(self):
        src = '"hello world"'
        col = collect_emissions(src)
        exp = [(E_STRING, "hello world")]

        self.assertEqual(col, exp)


    def test_quote_symbol(self):
        src = "'foo"
        col = collect_emissions(src)
        exp = [(E_QUOTE,),
               (E_SYMBOL, "foo")]

        self.assertEqual(col, exp)


    def test_quote_list(self):
        src = "'(foo bar)"
        col = collect_emissions(src)
        exp = [(E_QUOTE,),
               (E_OPEN, ),
               (E_SYMBOL, "foo"),
               (E_SYMBOL, "bar"),
               (E_CLOSE, )]

        self.assertEqual(col, exp)


    def test_quasi(self):
        src = "`bar"
        col = collect_emissions(src)
        exp = [(E_QUASI,),
               (E_SYMBOL, "bar")]

        self.assertEqual(col, exp)


    def test_unquote(self):
        src = ",baz"
        col = collect_emissions(src)
        exp = [(E_UNQUOTE,),
               (E_SYMBOL, "baz")]

        self.assertEqual(col, exp)


    def test_splice(self):
        src = "@qux"
        col = collect_emissions(src)
        exp = [(E_SPLICE,),
               (E_SYMBOL, "qux")]

        self.assertEqual(col, exp)


    def test_list(self):
        src = "(testing a thing)"
        col = collect_emissions(src)
        exp = [(E_OPEN,),
               (E_SYMBOL, "testing"),
               (E_SYMBOL, "a"),
               (E_SYMBOL, "thing"),
               (E_CLOSE,)]

        self.assertEqual(col, exp)


    def test_dot(self):
        src = "(testing . 123)"
        col = collect_emissions(src)
        exp = [(E_OPEN,),
               (E_SYMBOL, "testing"),
               (E_DOT,),
               (E_NUMBER, "123"),
               (E_CLOSE,)]

        self.assertEqual(col, exp)


    def test_newline(self):
        src = """
        ( this is
        a test )
        """
        col = collect_emissions(src)
        exp = [(E_NEWLINE,),
               (E_OPEN,),
               (E_SYMBOL, "this"),
               (E_SYMBOL, "is"),
               (E_NEWLINE,),
               (E_SYMBOL, "a"),
               (E_SYMBOL, "test"),
               (E_CLOSE,),
               (E_NEWLINE,)]

        self.assertEqual(col, exp)


    def test_newline(self):
        src = """
        ; Let's check out the comments
        ( this is ; well it's something
        a test ) ; this ought to work
        """
        col = collect_emissions(src)
        exp = [(E_NEWLINE,),
               (E_COMMENT, "; Let's check out the comments"),
               (E_NEWLINE,),
               (E_OPEN,),
               (E_SYMBOL, "this"),
               (E_SYMBOL, "is"),
               (E_COMMENT, "; well it's something"),
               (E_NEWLINE,),
               (E_SYMBOL, "a"),
               (E_SYMBOL, "test"),
               (E_CLOSE,),
               (E_COMMENT, "; this ought to work"),
               (E_NEWLINE,)]

        self.assertEqual(col, exp)


#
# The end.
