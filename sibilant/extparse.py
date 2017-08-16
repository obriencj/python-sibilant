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
Macro-enabled parser

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


from . import symbol, keyword, cons, nil, is_pair

from enum import Enum
from functools import partial
from io import StringIO, IOBase
from re import compile as regex


__all__ = (
    "set_reader_macro", "read", "position_of",
    "ReaderStream",
)


_quote_sym = symbol("quote")
_quasiquote_sym = symbol("quasiquote")
_unquote_sym = symbol("unquote")
_unquotesplicing_sym = symbol("unquote-splicing")
_splice_sym = symbol("splice")


class Event(Enum):
    DOT = object()
    CLOSE_PAREN = object()


class ReaderSyntaxError(SyntaxError):
    def __init__(self, msg, position):
        super().__init__(msg)
        self.position = position


class Reader(object):

    def __init__(self):
        self.reader_macros = {}
        self.positions = {}
        self.terminating = ["\n", "\r", "\t", " "]
        self._terms = "".join(self.terminating)


    def position_of(self, value):
        return self.positions.get(id(value))


    def read(self, stream):
        if isinstance(stream, str):
            stream = StringIO(stream)

        if isinstance(stream, IOBase):
            stream = ReaderStream(stream)

        return self._read(stream)


    def _read(self, stream):
        for c in iter(stream):
            if c.isspace():
                continue
            else:
                break
        else:
            # end of stream without a non-whitespace value
            return None

        position = stream.position()
        macro = self.reader_macros.get(c, self._read_atom)

        value = macro(stream, c)
        if is_pair(value):
            self.positions[id(value)] = value

        return value


    def set_macro(self, char, macro_fn, terminating=False):
        for c in char:
            self.reader_macros[c] = macro_fn
            if terminating:
                self.terminating.append(c)

        self._terms = "".join(self.terminating)


    def add_default_macros(self):
        sm = self.set_macro

        sm("()", self._read_pair, True)
        sm('""', self._read_string, True)
        sm("'", self._read_quote, True)
        sm("`", self._read_quasiquote, True)
        sm(",", self._read_unquote, True)
        sm("@", self._read_splice, True)
        sm(";", self._read_comment, True)


    def _read_atom(self, stream, c):
        atom = c + stream.read_until(self._terms.__contains__)

        if atom == ".":
            return Event.DOT
        elif decimal_like(atom):
            return as_decimal(atom)
        elif fraction_like(atom):
            return as_fraction(atom)
        elif complex_like(atom):
            return as_complex(atom)
        elif keyword_like(atom):
            return keyword(atom)
        else:
            return symbol(atom)


    def _read_pair(self, stream, char):
        if char == ")":
            return Event.CLOSE_PAREN

        position = stream.position()
        head = self._read(stream)

        if head is Event.CLOSE_PAREN:
            # terminate the proper list
            return nil

        elif head is Event.DOT:
            # improper list, the next item is the tail
            tail = self._read(stream)

            # test for syntax error -- there shouldn't be anything
            # left in the list syntax after the dot and tail
            position = stream.position()
            rest = self._read(stream)
            if rest is not Event.CLOSE_PAREN:
                raise ReaderSyntaxError("invalid use of .", position)

            return tail

        else:
            # continue the list
            value = cons(head, self._read_pair(stream, char))
            self.positions[id(value)] = position
            return value


    def _read_string(self, stream, char):
        pass


    def _read_quote(self, stream, char):
        return cons(_quote_sym, self.read(stream), nil)


    def _read_quasiquote(self, stream, char):
        return cons(_quasiquote_sym, self.read(stream), nil)


    def _read_unquote(self, stream, char):
        child = self.read(stream)
        if is_pair(child) and child[0] is _splice_sym:
            return cons(_unquotesplicing_sym, child[1])
        else:
            return cons(_unquote_sym, child, nil)


    def _read_splice(self, stream, char):
        return cons(_splice_sym, self.read(stream), nil)


    def _read_comment(self, stream, char):
        pass


class ReaderStream(object):

    def __init__(self, stream):
        self.stream = stream
        self.lin = 1
        self.col = -1


    def position(self):
        return self.lin, self.col


    def read(self, count=1):
        data = self.stream.read(count)

        lin = self.lin
        col = self.col

        for c in data:
            if c == "\n":
                lin += 1
                col = -1
                continue
            elif c == "\r":
                col = -1
                continue
            else:
                col += 1
                continue

        self.lin = lin
        self.col = col
        return data


    def __iter__(self):
        return iter(partial(self.read, 1), '')


    def read_eol(self):
        return self.read_until("\n\r".__contains__)


    def read_until(self, testf):
        stream = self.stream
        start = stream.tell()

        for c in iter(partial(stream.read, 1), ''):
            if testf(c):
                end = stream.tell() - 1
                break
        else:
            end = stream.tell()

        stream.seek(start, 0)

        # note, all the above seeking works on the stream directly,
        # and then resets it. We call self.read() here so that the
        # col/lineno accumulators can be updated.
        return self.read(end - start)


    def tell(self):
        return self.stream.tell()


    def seek(self, offset, whence):
        self.stream.seek(offset, whence)


def setup_reader():
    reader = Reader()
    reader.add_default_macros()

    return reader.set_macro, reader.read, reader.position_of


set_reader_macro, read, position_of = setup_reader()

decimal_like = regex(r"-?(\d*\.?\d+|\d+\.?\d*)").match

fraction_like = regex(r"\d+/\d+").match

complex_like = regex(r"-?\d*\.?\d+\+\d*\.?\d*[ij]").match

keyword_like = regex(r"^(:.+|.+:)$").match


#
# The end.
