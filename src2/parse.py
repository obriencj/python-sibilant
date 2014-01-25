"""

Simple event-emitting Sexp parser.

"""



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
    for c in stream_chars(stream):
        if c in "\n\r":
            yield (E_NEWLINE,)
            
        elif c.isspace():
            continue

        elif c.isdigit():
            stream_unread(stream)
            yield (E_NUMBER, parse_token(stream))

        elif c == "(":
            yield (E_OPEN, )

        elif c == ")":
            yield (E_CLOSE, )

        elif c == ".":
            yield (E_DOT, )

        elif c == "#":
            #stream_unread(stream)
            #yield (E_SHARP, parse_token(stream))
            yield (E_SHARP, )

        elif c == "\"":
            yield (E_STRING, parse_string(stream))

        elif c == "'":
            yield (E_QUOTE,)

        elif c == "`":
            yield (E_QUASI,)

        elif c == ",":
            yield (E_UNQUOTE,)

        elif c == "@":
            yield (E_SPLICE,)        

        elif c == ";":
            stream_unread(stream)
            yield (E_COMMENT, parse_comment(stream))

        else:
            stream_unread(stream)
            yield (E_SYMBOL, parse_token(stream))



def stream_chars(stream):
    c = stream.read(1)
    while c:
        yield c
        c = stream.read(1)



def stream_unread(stream):
    stream.seek(-1,1)



def read_until(stream, testf):
    from cStringIO import StringIO
    tok = StringIO()

    for c in stream_chars(stream):
        if testf(c):
            stream_unread(stream)
            break
        
        else:
            tok.write(c)

    return tok.getvalue()
    


def parse_token(stream):
    return read_until(stream, lambda c: c.isspace() or c in "()")



def parse_comment(stream):
    return read_until(stream, lambda c: c in "\n\r")



# this is using c-style escapes. I need to convert it into
# scheme-style, which would be #\Newline instead of \n
def parse_string(stream):
    from cStringIO import StringIO
    tok = StringIO()
    esc = False

    for c in stream_chars(stream):
        if (not esc) and c == "\"":
            break

        tok.write(c)
        esc = (not esc) and c == "\\"

    return tok.getvalue()



def _test():
    from cStringIO import StringIO

    srcl = ("testing",
            "(testing a thing)",
            "'a",
            "`a",
            "#t",
            r"#\newline (foo)",
            ",@(foo)",
            '''"hello world"''',
            "; a comment \n;() \n;'a")

    for s in srcl:
        print "testing: %r" % s
        for e in parse(StringIO(s)):
            print " ", e

    # done



if __name__ == "__main__":
    _test()



#
# The end.
