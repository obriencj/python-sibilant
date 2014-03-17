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
Simple event-emitting Sexp parser for Sibilant

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


from functools import partial


__all__ = ( "parse",
            "E_SYMBOL", "E_NUMBER", "E_SHARP", "E_STRING",
            "E_QUOTE", "E_QUASI", "E_UNQUOTE", "E_SPLICE",
            "E_OPEN", "E_CLOSE", "E_DOT", "E_COMMENT",
            "E_NEWLINE" )


# these are the events that can be emitted.
E_SYMBOL = "symbol"
E_NUMBER = "number"
E_SHARP = "sharp"
E_STRING = "string"
E_QUOTE = "quote"
E_QUASI = "quasi"
E_UNQUOTE = "unquote"
E_SPLICE = "splice"
E_OPEN = "open-paren"
E_CLOSE = "close-paren"
E_DOT = "dot"
E_COMMENT = "comment"
E_NEWLINE = "newline"


def parse(stream):
    """
    generator emitting tuples in the form `(event, (line_number,
    col_number), *event_data)`
    """

    lin = 1
    col = -1

    for c in stream_chars(stream):
        col += 1

        if c == "\n":
            yield (E_NEWLINE, (lin, col))
            lin += 1
            col = -1
            continue

        elif c == "\r":
            col = -1
            continue

        elif c.isspace():
            continue

        elif c.isdigit():
            stream_unread(stream)

            r = stream.tell()
            t = parse_token(stream)
            yield (E_NUMBER, (lin, col), t)
            col += (stream.tell() - r) - 1
            continue

        elif c == "(":
            yield (E_OPEN, (lin, col))
            continue

        elif c == ")":
            yield (E_CLOSE, (lin, col))
            continue

        elif c == ".":
            yield (E_DOT, (lin, col))
            continue

        elif c == "#":
            yield (E_SHARP, (lin, col))
            continue

        elif c == "\"":
            r = stream.tell()
            t = parse_string(stream)
            yield (E_STRING, (lin, col), t)
            col += (stream.tell() - r)
            continue

        elif c == "'":
            yield (E_QUOTE, (lin, col))
            continue

        elif c == "`":
            yield (E_QUASI, (lin, col))
            continue

        elif c == ",":
            yield (E_UNQUOTE, (lin, col))
            continue

        elif c == "@":
            yield (E_SPLICE, (lin, col))
            continue

        elif c == ";":
            stream_unread(stream)

            r = stream.tell()
            t = parse_comment(stream)
            yield (E_COMMENT, (lin, col), t)
            col += (stream.tell() - r) - 1
            continue

        else:
            stream_unread(stream)

            r = stream.tell()
            t = parse_token(stream)
            yield (E_SYMBOL, (lin, col), t)
            col += (stream.tell() - r) - 1
            continue


def stream_chars(stream):
    """
    iterate over stream one character at a time
    """

    return iter(partial(stream.read, 1), '')


def stream_unread(stream, count=1):
    """
    rewinds stream count (default=1) characters
    """

    stream.seek(stream.tell()-count, 0)


def read_until(stream, testf):
    """
    returns string of characters from stream up until testf(c) passes
    """

    start = stream.tell()

    for c in stream_chars(stream):
        if testf(c):
            stream_unread(stream)
            break

    end = stream.tell()
    stream.seek(start, 0)
    return stream.read(end - start)


def parse_token(stream):
    return read_until(stream, lambda c: c.isspace() or c in "()")


def parse_comment(stream):
    return read_until(stream, lambda c: c in "\n\r")


# this is using c-style escapes. I need to convert it into
# scheme-style, which would be #\Newline instead of \n
def parse_string(stream):
    start = stream.tell()
    esc = False

    c = stream.read(1)
    for c in stream_chars(stream):
        if (not esc) and c == '\"':
            stream_unread(stream)
            break
        esc = (not esc) and c == '\\'

    end = stream.tell()

    # rewind and get the string contents in a single read
    stream.seek(start, 0)
    value = stream.read(end - start)

    # discard the closing sentinel
    stream.read(1)

    return value


#
# The end.
