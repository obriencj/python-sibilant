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

from contextlib import contextmanager
from functools import partial
from io import StringIO, IOBase
from re import compile as regex


__all__ = (
    "SibilantSyntaxError", "ReaderSyntaxError",
    "SourceStream", "source_open", "source_str", "source_stream",
    "Reader", "default_reader",
)


_quote_sym = symbol("quote")
_quasiquote_sym = symbol("quasiquote")
_unquote_sym = symbol("unquote")
_unquotesplicing_sym = symbol("unquote-splicing")
_splice_sym = symbol("splice")
_fraction_sym = symbol("fraction")


_integer_like = partial(regex(r"-?\d+").match)

_hex_like = partial(regex(r"0x[\da-f]+").match)

_oct_like = partial(regex(r"0o[0-7]+").match)

_bin_like = partial(regex(r"0b[01]+").match)

_decimal_like = partial(regex(r"-?(\d*\.\d+|\d+\.\d*)").match)

_fraction_like = partial(regex(r"-?\d+/\d+").match)

_complex_like = partial(regex(r"-?\d*\.?\d+\+\d*\.?\d*[ij]").match)

_keyword_like = partial(regex(r"^(:.+|.+:)$").match)

_as_decimal = partial(float)

_as_integer = partial(int)

_as_hex = partial(int, base=16)

_as_oct = partial(int, base=8)

_as_bin = partial(int, base=2)


def _as_fraction(s):
    return cons(_fraction_sym, s, nil)


def _as_complex(s):
    if s[-1] == "i":
        return complex(s[:-1] + "j")
    else:
        return complex(s)


VALUE = keyword("value")
DOT = keyword("dot")
SKIP = keyword("skip")
CLOSE_PAREN = keyword("close-pair")
EOF = keyword("eof")


class SibilantSyntaxError(SyntaxError):
    def __init__(self, message, location=None, filename=None):
        self.message = message
        self.filename = filename
        if location:
            self.lineno, self.offset = location


class ReaderSyntaxError(SibilantSyntaxError):
    pass


class Reader(object):

    def __init__(self, nodefaults=False):
        self.reader_macros = {}
        self.atom_patterns = []
        self.terminating = ["\n", "\r", "\t", " "]
        self._terms = "".join(self.terminating)

        if not nodefaults:
            self._add_default_macros()
            self._add_default_atoms()


    def read(self, reader_stream):
        """
        Returns a cons cell, symbol, or numeric value. Returns None if no
        data left in stream. Raises ReaderSyntaxError to complain
        about syntactic difficulties in the stream.
        """

        if isinstance(reader_stream, str):
            reader_stream = StringIO(reader_stream)

        if isinstance(reader_stream, IOBase):
            reader_stream = ReaderStream(reader_stream)

        event, pos, value = self._read(reader_stream)

        if event is VALUE:
            return value
        elif event is EOF:
            # TODO: could probably raise error?
            return None
        else:
            raise reader_stream.error("invalid syntax", pos)


    def _read(self, stream):
        while True:
            stream.skip_whitespace()

            position = stream.position()
            c = stream.read()
            if not c:
                return EOF, position, None

            macro = self.reader_macros.get(c, self._read_atom)

            event, value = macro(stream, c)

            # print("_read:", event, position, value)

            if event is SKIP:
                continue
            else:
                break

        # record cons cell locations in the positions map
        if is_pair(value):
            stream.record_position(value, position)

        return event, position, value


    def set_event_macro(self, char, macro_fn, terminating=False):
        for c in char:
            self.reader_macros[c] = macro_fn
            if terminating:
                self.terminating.append(c)

        self._terms = "".join(self.terminating)


    def get_event_macro(self, char):
        try:
            return (self.reader_macros[char], char in self.terminating)
        except KeyError:
            return None


    def clear_event_macro(self, char):
        if char in self.reader_macros:
            self.terminating.remove(char)
            self._terms = "".join(self.terminating)
            del self.reader_macros[char]


    @contextmanager
    def temporary_event_macro(self, char, macro_fn, terminating=False):
        old = self.get_event_macro(char)
        self.set_event_macro(char, macro_fn, terminating)

        yield self

        if old is None:
            self.clear_event_macro(char)
        else:
            self.set_event_macro(char, *old)


    def set_macro_character(self, char, macro_fn, terminating=False):
        def macro_adapter(stream, char):
            return VALUE, macro_fn(stream, char)
        self.set_event_macro(char, macro_adapter, terminating)


    def _add_default_macros(self):
        sm = self.set_event_macro

        sm("(", self._read_pair, True)
        sm(")", self._close_paren, True)
        sm('"', self._read_string, True)
        sm("'", self._read_quote, True)
        sm("`", self._read_quasiquote, True)
        # sm(",", self._read_unquote, True)
        # sm("@", self._read_splice, True)
        sm(";", self._read_comment, True)


    def set_atom_pattern(self, namesym, match_fn, conversion_fn):
        for patt in self.atom_patterns:
            if patt[0] is namesym:
                patt[1] = match_fn
                patt[2] = conversion_fn
                break
        else:
            self.atom_patterns.insert(0, [namesym, match_fn, conversion_fn])


    def get_atom_pattern(self, namesym):
        for patt in self.atom_patterns:
            if patt[0] is namesym:
                return patt
        else:
            return None


    def _add_default_atoms(self):
        ap = self.set_atom_pattern

        ap(symbol("keyword"), _keyword_like, keyword)
        ap(symbol("int"), _integer_like, _as_integer)
        ap(symbol("hex"), _hex_like, _as_hex)
        ap(symbol("oct"), _oct_like, _as_oct)
        ap(symbol("binary"), _bin_like, _as_bin)
        ap(symbol("float"), _decimal_like, _as_decimal)
        ap(symbol("complex"), _complex_like, _as_complex)
        ap(symbol("fraction"), _fraction_like, _as_fraction)


    def _read_atom(self, stream, c):
        """
        The default character macro handler, for when nothing else has
        matched.
        """

        atom = c + stream.read_until(self._terms.__contains__)

        if atom == ".":
            return DOT, None

        for name, match, conv in self.atom_patterns:
            if match(atom):
                value = conv(atom)
                break
        else:
            value = symbol(atom)

        return VALUE, value


    def _read_pair(self, stream, char):
        """
        The character macro handler for pair notation
        """

        result = nil
        work = result

        while True:
            event, position, value = self._read(stream)

            if event is CLOSE_PAREN:
                if result is nil:
                    return VALUE, nil
                else:
                    work[1] = nil
                    return VALUE, result


            elif event is DOT:
                if result is nil:
                    # haven't put any items into the result yet, dot
                    # is therefore invalid.
                    raise stream.error("invalid dotted list",
                                       position)

                # improper list, the next item is the tail. Read it
                # and be done.
                event, tail_pos, tail = self._read(stream)
                if event is not VALUE:
                    raise stream.error("invalid list syntax",
                                       tail_pos)

                close_event, close_pos, _value = self._read(stream)
                if close_event is not CLOSE_PAREN:
                    raise stream.error("invalid use of dot in list",
                                       close_pos)

                work[1] = tail
                return VALUE, result

            elif event is EOF:
                raise stream.error("unexpected EOF")

            elif result is nil:
                # begin the list.
                result = cons(value, nil)
                work = result
                stream.record_position(work, position)
                continue

            else:
                # append to the current list
                work[1] = cons(value, nil)
                work = work[1]
                stream.record_position(work, position)
                continue

        return VALUE, result


    def _close_paren(self, stream, char):
        """
        The character macro handler for a closing parenthesis
        """

        return CLOSE_PAREN, None


    def _read_string(self, stream, char):
        """
        The character macro handler for string literals
        """

        combine = []

        esc = False

        c = ""
        for c in iter(stream):
            if (not esc) and c == '\"':
                # done deal
                break
            esc = (not esc) and c == '\\'
            combine.append(c)

        if c != '\"':
            raise stream.error("Unexpected EOF")

        combine = "".join(combine).encode()
        return VALUE, combine.decode("unicode-escape")


    def _read_quote(self, stream, char):
        """
        The character macro handler for quote
        """

        event, pos, child = self._read(stream)

        if event is not VALUE:
            msg = "invalid use of %s" % char
            raise stream.error(msg, pos)

        return VALUE, cons(_quote_sym, child, nil)


    def _read_quasiquote(self, stream, char):
        """
        The character macro handler for quasiquote
        """

        with self.temporary_event_macro(",", self._read_unquote, True):
            event, pos, child = self._read(stream)

        if event is not VALUE:
            msg = "invalid use of %s" % char
            raise stream.error(msg, pos)

        return VALUE, cons(_quasiquote_sym, child, nil)


    def _read_unquote(self, stream, char):
        """
        The character macro handler for unquote
        """

        with self.temporary_event_macro("@", self._read_splice, True):
            event, pos, child = self._read(stream)

        if event is not VALUE:
            msg = "invalid use of %s" % char
            raise stream.error(msg, pos)

        if is_pair(child) and child[0] is _splice_sym:
            value = cons(_unquotesplicing_sym, child[1])
        else:
            value = cons(_unquote_sym, child, nil)

        return VALUE, value


    def _read_splice(self, stream, char):
        """
        The character macro handler for splice
        """

        event, pos, child = self._read(stream)

        if event is not VALUE:
            msg = "invalid use of %s" % char
            raise stream.error(msg, pos)

        return VALUE, cons(_splice_sym, child, nil)


    def _read_comment(self, stream, char):
        """
        The character macro handler for comments
        """

        comment = stream.read_until("\n\r".__contains__)
        return SKIP, comment


@contextmanager
def source_open(filename):
    with open(filename, "rt") as fs:
        reader = SourceStream(fs, filename=filename)
        reader.skip_exec()
        yield reader


def source_str(source_str, filename=None):
    return SourceStream(StringIO(source_str))


def source_stream(source_stream, filename=None):
    return SourceStream(source_stream)


class SourceStream(object):

    def __init__(self, stream, filename=None):
        self.filename = filename
        self.stream = stream
        self.lin = 1
        self.col = 0
        self.positions = {}


    def position_of(self, value):
        return self.positions.get(id(value))


    def record_position(self, value, position=None):
        if position is None:
            position = self.lin, self.col

        self.positions[id(value)] = position
        return position


    def position(self):
        """
        The line and column of the next character to be read.

        Line numbers start from 1, columns start from 0
        """

        return self.lin, self.col


    def error(self, message, position=None):
        if position is None:
            position = self.lin, self.col

        return ReaderSyntaxError(message, position, filename=self.filename)


    def read(self, count=1):
        """
        This analyzes each actual read in order to perform line and column
        position counting.
        """

        data = self.stream.read(count)

        lin = self.lin
        col = self.col

        for c in data:
            if c == "\n":
                lin += 1
                col = 0
                continue
            elif c == "\r":
                col = 0
                continue
            else:
                col += 1
                continue

        self.lin = lin
        self.col = col

        return data


    def __iter__(self):
        return iter(partial(self.read, 1), '')


    def skip_exec(self):
        stream = self.stream
        start = stream.tell()

        if stream.read(2) == "#!":
            stream.readline()
            self.lin += 1
            self.col = 0
        else:
            stream.seek(start, 0)


    def skip_whitespace(self):
        return self.read_until(lambda c: not c.isspace())


    def read_until(self, testf):
        stream = self.stream
        start = stream.tell()

        index = 0
        for index, char in enumerate(iter(partial(stream.read, 1), '')):
            if testf(char):
                break
        else:
            index += 1

        stream.seek(start, 0)

        if index:
            # note, all the above seeking works on the stream directly,
            # and then resets it. We call self.read() here so that the
            # col/lineno accumulators can be updated.
            return self.read(index)

        else:
            return ""


default_reader = Reader()


#
# The end.
