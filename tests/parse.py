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

from sibilant.lib import cons, symbol, keyword, nil, car, cdr
from sibilant.parse import default_reader, source_str


def parse_source(src_str):
    reader = default_reader
    stream = source_str(src_str, "<unittest>")
    return reader.read(stream)


class Atomics(TestCase):


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


    def test_float(self):

        src = "1.5"
        col = parse_source(src)
        self.assertEqual(col, 1.5)

        src = "1."
        col = parse_source(src)
        self.assertEqual(col, 1.0)

        src = ".5"
        col = parse_source(src)
        self.assertEqual(col, 0.5)

        src = "1.5e2"
        col = parse_source(src)
        self.assertEqual(col, 1.5e2)

        src = ".5e2"
        col = parse_source(src)
        self.assertEqual(col, 0.5e2)

        src = "5e-1"
        col = parse_source(src)
        self.assertEqual(col, 5e-1)

        src = "1.5f"
        col = parse_source(src)
        self.assertEqual(col, 1.5)

        src = "1.f"
        col = parse_source(src)
        self.assertEqual(col, 1.0)

        src = ".5f"
        col = parse_source(src)
        self.assertEqual(col, 0.5)


    def test_complex(self):

        src = "1+5i"
        col = parse_source(src)
        self.assertEqual(col, complex("1+5j"))

        src = "1+5j"
        col = parse_source(src)
        self.assertEqual(col, complex("1+5j"))


    def test_fraction(self):

        src = "1/2/3"
        self.assertRaises(SyntaxError, parse_source, src)

        src = "1/2"
        col = parse_source(src)
        self.assertEqual(col, cons(symbol("fraction"),
                                   1, 2, nil))

        src = "-1/2"
        col = parse_source(src)
        self.assertEqual(col, cons(symbol("fraction"),
                                   -1, 2, nil))


    def test_decimal(self):
        src = "1.5d"
        col = parse_source(src)
        self.assertEqual(col, cons(symbol("decimal"),
                                   (0, (1, 5), -1), nil))

        src = ".5d"
        col = parse_source(src)
        self.assertEqual(col, cons(symbol("decimal"),
                                   (0, (5, ), -1), nil))

        src = "1.d"
        col = parse_source(src)
        self.assertEqual(col, cons(symbol("decimal"),
                                   (0, (1, ), 0), nil))


class Strings(TestCase):


    def test_string(self):
        src = '""'
        col = parse_source(src)
        self.assertEqual(col, "")

        src = ' "" '
        col = parse_source(src)
        self.assertEqual(col, "")

        src = r' "\"" '
        col = parse_source(src)
        self.assertEqual(col, "\"")

        src = '"hello world"'
        col = parse_source(src)
        self.assertEqual(col, "hello world")

        src = r' "hello\n \tworld" '
        col = parse_source(src)
        self.assertEqual(col, "hello\n \tworld")

        src = ' "hello\n \tworld" '
        col = parse_source(src)
        self.assertEqual(col, "hello\n \tworld")

        src = '"¿?"'
        col = parse_source(src)
        self.assertEqual(col, "¿?")

        src = '"¿\\n?"'
        col = parse_source(src)
        self.assertEqual(col, "¿\n?")


    def test_3string(self):
        src = r'""""""'
        col = parse_source(src)
        self.assertEqual(col, "")

        src = r' """ """ '
        col = parse_source(src)
        self.assertEqual(col, " ")

        src = r'"""hello world"""'
        col = parse_source(src)
        self.assertEqual(col, 'hello world')

        src = r'"""hello "favorite" world"""'
        col = parse_source(src)
        self.assertEqual(col, 'hello "favorite" world')

        src = r'"""hello \""" "\"" ""\" world"""'
        col = parse_source(src)
        self.assertEqual(col, 'hello """ """ """ world')

        src = r'"""hello "world\""""'
        col = parse_source(src)
        self.assertEqual(col, 'hello "world"')

        src = r' """hello\n \tworld""" '
        col = parse_source(src)
        self.assertEqual(col, "hello\n \tworld")

        src = ' """hello\n \tworld""" '
        col = parse_source(src)
        self.assertEqual(col, "hello\n \tworld")

        src = '"""¿?"""'
        col = parse_source(src)
        self.assertEqual(col, "¿?")

        src = r'"""¿\n?"""'
        col = parse_source(src)
        self.assertEqual(col, "¿\n?")


    def test_raw(self):
        src = r'r""'
        col = parse_source(src)
        self.assertEqual(col, "")

        src = r' r"" '
        col = parse_source(src)
        self.assertEqual(col, "")

        src = r' r"\"" '
        col = parse_source(src)
        self.assertEqual(col, r"\"")

        src = 'r"hello world"'
        col = parse_source(src)
        self.assertEqual(col, "hello world")

        src = r' r"hello\n \tworld" '
        col = parse_source(src)
        self.assertEqual(col, r"hello\n \tworld")

        src = r' r"hello\n \tworld" '
        col = parse_source(src)
        self.assertEqual(col, r"hello\n \tworld")

        src = r'r"¿?"'
        col = parse_source(src)
        self.assertEqual(col, r"¿?")

        src = r'r"¿\\n?"'
        col = parse_source(src)
        self.assertEqual(col, r"¿\\n?")


class Quoted(TestCase):


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

        src = "[testing a thing]"
        col = parse_source(src)
        self.assertEqual(col, exp)

        with self.assertRaises(SyntaxError):
            src = "(no way]"
            col = parse_source(src)

        with self.assertRaises(SyntaxError):
            src = "[no way)"
            col = parse_source(src)

        with self.assertRaises(SyntaxError):
            src = "{no way]"
            col = parse_source(src)

        with self.assertRaises(SyntaxError):
            src = "[no way}"
            col = parse_source(src)

        with self.assertRaises(SyntaxError):
            src = "{no way)"
            col = parse_source(src)

        with self.assertRaises(SyntaxError):
            src = "(no way}"
            col = parse_source(src)


class ConsPairs(TestCase):


    def test_implicit_begin(self):

        src = "{testing a thing}"
        col = parse_source(src)
        self.assertEqual(col, cons(symbol("begin"),
                                   symbol("testing"),
                                   symbol("a"),
                                   symbol("thing"),
                                   nil))


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
        strm = source_str(src, "<unittest>")
        read = default_reader.read

        a = read(strm)
        b = read(strm)
        c = read(strm)

        self.assertEqual(a, 1.0)
        self.assertEqual(b, "2")
        self.assertEqual(c, cons(3, nil))


class Positions(TestCase):


    def test_position_1(self):
        src = """
        (hello world)
        """
        strm = source_str(src, "<unittest>")
        expr = default_reader.read(strm)

        self.assertEqual(expr.get_position(), (2, 8))
        expr = cdr(expr)
        self.assertEqual(expr.get_position(), (2, 15))


    def test_position_2(self):
        src = """
        (hello (world)
         how are you)
        """
        strm = source_str(src, "<unittest>")
        expr = default_reader.read(strm)

        self.assertEqual(expr.get_position(), (2, 8))
        expr = cdr(expr)
        self.assertEqual(expr.get_position(), (2, 15))

        wld = car(expr)
        self.assertEqual(expr.get_position(), (2, 15))

        expr = cdr(expr)
        self.assertEqual(expr.get_position(), (3, 9))

        expr = cdr(expr)
        self.assertEqual(expr.get_position(), (3, 13))

        expr = cdr(expr)
        self.assertEqual(expr.get_position(), (3, 17))


#
# The end.
