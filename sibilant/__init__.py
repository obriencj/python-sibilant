#
#



def seq(*i):
    return i



def make_context():
    class Context(object):
        pass

    ctx = Context()
    
    ctx.symc = 0
    ctx.sym_format = "__i__%04x"

    return ctx



def gensym(context, memo=None):
    context.symc += 1
    ret = (context.sym_format % context.symc)
    if memo:
        ret = ret  + memo
    return ret



#def print_tree(tree, out):
#    out.write("(")
#    for n in tree:
#        if isinstance(n, list):
#            print_tree(n, out)
#        else:
#            out.write(n)
#            out.write(" ")
#    out.write(")")



def tree_to_str(tree):
    from cStringIO import StringIO
    buf = StringIO()
    print_tree(tree, buf)
    return buf.getvalue()



def compile_from_str(source_str):
    from cStringIO import StringIO
    from sibilant.parser import build_tree
    from sibilant.process import process_expression
    
    ctx = make_context()
    k = "lambda *x: (lambda *y: y)(None, *x)"

    t = build_tree(StringIO(source_str))
    o = [process_expression(ctx, k, i) for i in t]

    return o



def eval_loop(pyexpr, glbls):
    print "expression", pyexpr
    r = eval(pyexpr, glbls)
    while r and r[0]:
        print "r", r
        r = r[0](*r[1:])
    print "final r", r
    return r[1:]



def eval_from_str(source_str, glbls):
    expr = compile_from_str(source_str)
    ret = [eval_loop(e, glbls) for e in expr]
    print "eval_from_str", ret
    return ret



#
# The end.
