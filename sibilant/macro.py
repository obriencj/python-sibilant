"""
"""


from sibilant import seq



def macro_condl(items):
    if not items:
        return "#f"
    
    else:
        c, e = items[0]
        if c == "else":
            c = "#t"
        return ("if", c, e, macro_cond(items[1:]))



def macro_cond(*items):
    return macro_condl(items)



def macro_let_unnamed(bindings, *body):
    print ":::::: macro_let_unnamed", bindings
        
    vars = [k for k,v in bindings]
    vals = [v for k,v in bindings]
    
    return seq(seq("lambda", vars, *body), *vals)



def macro_let_named(name, bindings, *body):
    
    # XXX this is wrong. Need to either invoke via Y or use an interim
    # /name/ and setf it to the correct lambda
    
    print ":::::: macro_let_named", name, bindings
    
    vars = [k for k,v in bindings]
    vals = [v for k,v in bindings]

    return ("let", ((name, seq("lambda", vars, *body)),),
            seq(name, *vals))



def macro_let(*form):
    print ":::::: macro_let", form
    
    if isinstance(form[0], str):
        return macro_let_named(*form)
    else:
        return macro_let_unnamed(*form)



def macro_when(cond, *body):
    return ("if", cond, seq("begin", *body))



macros = {
    "cond": macro_cond,
    "let": macro_let,
    "when": macro_when,
    }



#
# The end.
