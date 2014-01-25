"""
This old thing again
"""



def next_fd_gen(fd):
    c = fd.read(1)
    while c:
        yield c
        c = fd.read(1)



def build_quoted(fd, quotec='\"'):
    from cStringIO import StringIO

    token = StringIO()
    esc = False

    token.write(quotec)
    for c in next_fd_gen(fd):
        if not esc and c == quotec:
            break
        else:
            token.write(c)
            esc = (c == '\\') and (not esc)

    token.write(quotec)
    return token.getvalue()



def build_tree(fd):

    """ returns a list of expressions parsed from fd """

    return [e for e in gen_exprs(fd)]



def gen_exprs(fd):
    from cStringIO import StringIO
    
    token = None
    
    for c in next_fd_gen(fd):
        if c in ';#/\"\'() \n\r\t':
            if token:
                yield token.getvalue()
                token = None
        
        else:
            if not token:
                token = StringIO()
            token.write(c)
            continue

        if c in ';#':
            # comments run to end of line
            for c in next_fd_gen(fd):
                if c in "\n\r":
                    break

        elif c == '(':
            yield build_tree(fd)

        elif c == ')':
            return

        elif c in '\'\"':
            yield build_quoted(fd, c)

    if token:
        yield token.getvalue()



#
# The end.
