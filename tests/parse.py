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

from sibilant import cons, symbol, keyword, nil
from sibilant.parse import default_reader, source_str


def parse_source(src_str):
    reader = default_reader
    stream = source_str(src_str)
    return reader.read(stream)


class TestParse(TestCase):


    def test_nothing(self):
        src = ""
        col = parse_source(src)
        self.assertIs(col, None)


    def test_symbol(self):
        src = "lambda"
        col = parse_source(src)
        self.assertIs(col, symbol("lambda"))

        src = "Number123"
        col = parse_source(src)
        self.assertIs(col, symbol("Number123"))

        src = "None"
        col = parse_source(src)
        self.assertIs(col, symbol("None"))

        src = "True"
        col = parse_source(src)
        self.assertIs(col, symbol("True"))

        src = "False"
        col = parse_source(src)
        self.assertIs(col, symbol("False"))

        src = "nil"
        col = parse_source(src)
        self.assertIs(col, symbol("nil"))

        src = "..."
        col = parse_source(src)
        self.assertIs(col, symbol("..."))

        src = "taco:bell"
        col = parse_source(src)
        self.assertIs(col, symbol("taco:bell"))


    def test_keyword(self):
        src = ":x"
        col = parse_source(src)
        self.assertIs(col, keyword("x"))

        src = "x:"
        col = parse_source(src)
        self.assertIs(col, keyword("x"))

        src = ":x:"
        col = parse_source(src)
        self.assertIs(col, keyword("x"))
        src = ":x"
        col = parse_source(src)
        self.assertIs(col, keyword("x"))

        src = "x:"
        col = parse_source(src)
        self.assertIs(col, keyword("x"))

        src = ":number-1"
        col = parse_source(src)
        self.assertIs(col, keyword("number-1"))

        src = "number-1:"
        col = parse_source(src)
        self.assertIs(col, keyword("number-1"))


    def test_number(self):
        src = "123"
        col = parse_source(src)
        self.assertEqual(col, 123)

        src = "-9"
        col = parse_source(src)
        self.assertEqual(col, -9)

        src = "0xfe"
        col = parse_source(src)
        self.assertEqual(col, 0xfe)

        src = "0o74"
        col = parse_source(src)
        self.assertEqual(col, 0o74)

        src = "0b101"
        col = parse_source(src)
        self.assertEqual(col, 0b101)

        src = "1.5"
        col = parse_source(src)
        self.assertEqual(col, 1.5)

        src = "1+5i"
        col = parse_source(src)
        self.assertEqual(col, complex("1+5j"))

        src = "1+5j"
        col = parse_source(src)
        self.assertEqual(col, complex("1+5j"))

        src = "1/2"
        col = parse_source(src)
        self.assertEqual(col, cons(symbol("fraction"),
                                   "1/2", nil))

        src = "-1/2"
        col = parse_source(src)
        self.assertEqual(col, cons(symbol("fraction"),
                                   "-1/2", nil))


    def test_string(self):
        src = '""'
        col = parse_source(src)
        self.assertEqual(col, "")

        src = ' "" '
        col = parse_source(src)
        self.assertEqual(col, "")

        src = '"hello world"'
        col = parse_source(src)
        self.assertEqual(col, "hello world")

        src = ' "hello\\n \\tworld" '
        col = parse_source(src)
        self.assertEqual(col, "hello\n \tworld")


    def test_quote_symbol(self):
        src = """
        'foo
        """
        col = parse_source(src)
        self.assertEqual(col, cons(symbol("quote"),
                                   symbol("foo"),
                                   nil))


    def test_quote_list(self):
        src = """
        '(foo bar)
        """
        col = parse_source(src)
        self.assertEqual(col, cons(symbol("quote"),
                                   cons(symbol("foo"),
                                        symbol("bar"),
                                        nil),
                                   nil))

        src = """
        \n'(foo\n bar\n)
        """
        col = parse_source(src)
        self.assertEqual(col, cons(symbol("quote"),
                                   cons(symbol("foo"),
                                        symbol("bar"),
                                        nil),
                                   nil))


    def test_quasi(self):
        src = """
        `bar
        """
        col = parse_source(src)
        self.assertEqual(col, cons(symbol("quasiquote"),
                                   symbol("bar"),
                                   nil))


    def test_quote_unquote_splice(self):
        src = """
        `(,@foo)
        """
        col = parse_source(src)
        exp = cons(symbol("quasiquote"),
                   cons(cons(symbol("unquote-splicing"),
                             symbol("foo"),
                             nil),
                        nil),
                   nil)
        self.assertEqual(col, exp)

        src = """
        `(,@(foo bar))
        """
        col = parse_source(src)
        exp = cons(symbol("quasiquote"),
                   cons(cons(symbol("unquote-splicing"),
                             cons(symbol("foo"),
                                  symbol("bar"),
                                  nil),
                             nil),
                        nil),
                   nil)
        self.assertEqual(col, exp)


    def test_list(self):
        src = "(testing a thing)"
        col = parse_source(src)
        exp = cons(symbol("testing"),
                   symbol("a"),
                   symbol("thing"),
                   nil)

        self.assertEqual(col, exp)


    def test_dot(self):
        src = "(testing . 123)"
        col = parse_source(src)
        exp = cons(symbol("testing"), 123)

        self.assertEqual(col, exp)


    def test_newline(self):
        src = """
        ( this is
        a test )
        """
        col = parse_source(src)
        exp = cons(symbol("this"),
                   symbol("is"),
                   symbol("a"),
                   symbol("test"),
                   nil)

        self.assertEqual(col, exp)


    def test_comments(self):
        src = """
        ; Let's check out the comments
        ( this is ; well it's something
        a test ) ; this ought to work
        """
        col = parse_source(src)
        exp = cons(symbol("this"),
                   symbol("is"),
                   symbol("a"),
                   symbol("test"),
                   nil)

        self.assertEqual(col, exp)


    def test_multi(self):
        src = """
        1.0 "2" (3)
        """
        strm = source_str(src)
        read = default_reader.read

        a = read(strm)
        b = read(strm)
        c = read(strm)

        self.assertEqual(a, 1.0)
        self.assertEqual(b, "2")
        self.assertEqual(c, cons(3, nil))


#
# The end.
