"""

AST style parser

author: siege@preoccupied.net

"""



from compiler.ast import Node



class Compound(Node):
    def __init__(self, *items):
        self.items = items



class Atom(Node):
    def __init__(self, s):
        self.string = s



class Special(Compound):
    def __init__(self, name, *items):
        self.name = name
        self.items = items



class Operator(Compound):
    def __init__(self, op, *items):
        self.op = op
        self.items = items



class And(Special):
    pass



class Append(Special):
    pass



class Begin(Special):
    pass



class Call_CC(Special):
    pass



class Car(Special):
    pass



class Cdr(Special):
    pass



class Comment(Node):
    def __init__(self, s):
        self.string = s



class Cond(Special):
    pass



class Cond(Special):
    pass



class Eval(Special):
    pass



class If(Special):
    pass



class List(Special):
    pass



class Lambda(Special):
    pass



class Not(Special):
    pass



class Null_(Special):
    pass



class Number(Atom):
    pass



class Or(Special):
    pass



class Py_Function(Special):
    pass



class QuasiQuote(Special):
    pass



class Quote(Special):
    pass



class String(Atom):
    pass



class Symbol(Atom):
    pass



class Unquote(Special):
    pass



class Unquote_Splicing(Special):
    pass



def special_name(n):
    if n[-1] == '_':
        n = n[:-1]+"?"
    return n.lower().replace("_","-")



specials = {}


for n, v in globals():
    if v and issubclass(v.__class__, Special):
        specials[special_name(n)] = v



# Some examples of expressions and their expected result

# (lambda (a) a)
# Lambda(List(Symbol("a")), Symbol("a"))

# '()
# Quote(List())

# `(,a ,b c)
# QuasiQuote(List(UnQuote(Symbol("a")), UnQuote(Symbol("b")), Symbol("c")))



def fd_read_gen(fd):
    c = fd.read(1)
    while c:
        yield c
        c = fd.read(1)



def fd_unread(fd):
    fd.seek(-1, 1)



def build_string(fd):
    from cStringIO import StringIO

    token = StringIO()
    esc = False

    for c in fd_read_gen(fd):
        if not esc and c == '\"':
            break
        
        else:
            token.write(c)
            esc = (c == '\\') and (not esc)

    return String(token.getvalue())



def build_atom(fd):
    from cStringIO import StringIO

    token = StringIO()

    for c in fd_read_gen(fd):
        if c == ")":
            fd_unread(fd)
            break
        
        elif c.isspace():
            break

        else:
            token.write(c)

    tstr = token.getvalue()
    
    if tstr[0].isdigit():
        return Number(tstr)
    
    else:
        return Symbol(tstr)



def build_quote(fd):
    return Quote(build_expression(fd))



def build_quasiquote(fd):
    return QuasiQuote(build_expression(fd))



def build_unquote(fd):
    return Unquote(build_expression(fd))



def collapse_compound(comp):
    head = comp.items[0]
    tail = comp.items[1:]
    
    if isinstance(head, Symbol):
        klass = specials.get(head.name)
        if klass:
            return klass(*tail)
        
    return Eval(*comp.items)



def build_compound(fd):
    l = []
    
    for c in fd_read_gen(fd):
        if c.isspace():
            pass
        elif c == ")":
            break
        else:
            fd_unread(fd)
            l.append(build_expression(fd))

    return Compound(*l)



def build_comment(fd):
    from cStringIO import StringIO

    com = StringIO()
    
    for c in fd_read_gen(fd):
        if c in "\n\r":
            break
        
        else:
            com.write(c)

    return Comment(com.getvalue())



def build_expression(fd):
    c = fd.read(1)

    if c == ';':
        return build_comment(fd)
    
    elif c == '(':
        return build_compound(fd)
    
    elif c == "\'":
        return build_quote(fd)
        
    elif c == "`":
        return build_quasiquote(fd)

    elif c == ',':
        return build_unquote(fd)
        
    elif c == '\"':
        return build_string(fd)
    
    else:
        fd_unread(fd)
        return build_atom(fd)



def gen_parse_stream(fd):
    while not fd.closed:
        yield build_expression(fd)



def parse_stream(fd):
    return [e for e in gen_parse_stream(fd)]



def gen_parse_string(s):
    from cStringIO import StringIO
    return gen_parse_stream(StringIO(s))



def parse_string(s):
    return [e for e in gen_parse_string(s)]



#
# The end.
