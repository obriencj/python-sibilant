#
#


from compiler.ast import *
from sibilant import gensym, seq



def p_lambda(vars, bodystr):
    return "lambda %s: %s" % (",".join(vars), bodystr)



def process_list(ctx, k, elist):
    # this function is called to create an ordered cont pair
    # generating lambda from a list of expressions

    # zip expressions in the list and symbols they'll store into
    #syms = ["_i_%04x" % i for i in xrange(0, len(elist))]
    syms = [gensym(ctx) for i in elist]
    pairs = zip(elist, syms)
    
    # go backwards, the last symbol's continuation is cont_expr, and
    # the value is the symbols list
    nk = "(%s, %s)" % (k, ",".join(syms))
    alist = process_steps(ctx, nk, pairs)
    return alist



def make_process_op(sym):
    return lambda ctx,k,a,b: process_op(ctx,k,sym,a,b)



def process_op(ctx, k, op, a, b):
    g1, g2 = gensym(ctx), gensym(ctx)
    nk = p_lambda((g1,g2), "(%s, (%s %s %s))" % (k, g1, op, g2))
    return process_list(ctx, nk, (a, b))



def process_and(ctx, k, a, b):
    g1, g2 = gensym(ctx), gensym(ctx)
    ba = p_lambda((g1,), process_expression(ctx, g1, g2))
    bc = p_lambda((g1,), process_expression(ctx, g1, b))
    nk = p_lambda((g2,), "(%s, %s)[not %s](%s)" % (bc, ba, g2, k))
    
    return process_expression(ctx, nk, a)



def process_or(ctx, k, a, b):
    g1, g2 = gensym(ctx), gensym(ctx)
    ba = p_lambda((g1,), process_expression(ctx, g1, g2))
    bc = p_lambda((g1,), process_expression(ctx, g1, b))
    nk = p_lambda((g2,), "(%s, %s)[not %s](%s)" % (ba, bc, g2, k))
    
    return process_expression(ctx, nk, a)



def process_not(ctx, k, a):
    g1 = gensym(ctx)
    nk = p_lambda((g1,), "(%s, not %s)" % (k, g1))
    return process_expression(ctx, nk, a)



def process_begin(ctx, k, *body):
    gs = gensym(ctx)
    nk = "lambda *%s: (%s, %s[-1])" % (gs, k, gs)
    return process_list(ctx, nk, body)



def process_callcc(ctx, k, l):
    g1, g2 = gensym(ctx), gensym(ctx)
    
    fk = "lambda %s: (lambda %s: (%s, %s(%s)))(%s)" % \
         (g1, g2, g2, g1, g2, k)
    
    return process_expression(ctx, fk, l)



def process_if(ctx, k, cond, a, b):
    g1, g2 = gensym(ctx), gensym(ctx)
    
    ac = p_lambda((g1,), process_expression(ctx, g1, a))
    bc = p_lambda((g1,), process_expression(ctx, g1, b))
    nk = p_lambda((g2,), "(%s, %s)[not %s](%s)" % (ac, bc, g2, k))
    
    return process_expression(ctx, nk, cond)



def process_lambda(ctx, k, args, *body):
    g1 = gensym(ctx)
    bd = (process_begin(ctx, g1, *body))
    return "(%s, lambda %s: %s)" % \
           (k, ",".join(seq(g1, *args)), bd)



def process_pyfun(ctx, k, pyfunc):
    g1, g2 = gensym(ctx), gensym(ctx)
    return "(%s, lambda %s,*%s: (%s, %s(*%s)))" % \
           (k, g1, g2, g1, pyfunc, g2)



def process_eval(ctx, k, *elist):
    syms = [gensym(ctx) for i in elist]

    eval_to = "lambda %s: %s((%s),%s)" % \
              (",".join(syms), syms[0], k, ",".join(syms[1:]))

    return process_list(ctx, eval_to, elist)



specials = {
    "+": make_process_op("+"),
    "*": make_process_op("*"),
    "**": make_process_op("**"),
    "-": make_process_op("-"),
    "/": make_process_op("/"),
    "%": make_process_op("%"),
    "==": make_process_op("=="),
    "!=": make_process_op("!="),
    ">": make_process_op(">"),
    ">=": make_process_op(">="),
    "<": make_process_op("<"),
    "<=": make_process_op("<="),
    
    "and": process_and,
    "begin": process_begin,
    "callcc": process_callcc,
    "eval": process_eval,
    "if": process_if,
    "lambda": process_lambda,
    "not": process_not,
    "or": process_or,
    "py-fun": process_pyfun,
    }



def process_steps(ctx, k, pairs):
    print "process_steps", pairs
    
    if not pairs:
        return k

    e, s = pairs[-1]

    if isinstance(e, str):
        mystep = "(lambda %s: %s, %s)" % (s, k, e)
        
    else:
        mk = "(lambda %s: %s)" % (s, k)
        mystep = process_expression(ctx, mk, e)
        
    return process_steps(ctx, mystep, pairs[:-1])



def process_token(ctx, k, token):
    return "(%s, %s)" % (k, token)



def process_compound(ctx, k, elist):
    from sibilant.macro import macros
        
    head = elist[0]
    tail = elist[1:]
    
    if isinstance(head, str):
        if macros.has_key(head):
            expand = macros.get(head)(*tail)
            return process_expression(ctx, k, expand)
        
        elif specials.has_key(head):
            return specials.get(head)(ctx, k, *tail)

    return process_eval(ctx, k, head, *tail) 



def process_expression(ctx, k, expr):
    print "process_expression", expr

    if isinstance(expr, str):
        return process_token(ctx, k, expr)

    else:
        return process_compound(ctx, k, expr)



#
# The end.
