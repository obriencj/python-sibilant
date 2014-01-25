

def a(k):
    return (k, [False])

def b(k):
    return (k, [True])

def ae(k):
    return (k, ["AE"])

def be(k):
    return (k, ["BE"])

def c(k):
    return (k, ["C"])


def funcall(fn, k=lambda x:(None,x), *args):
    z, v = fn(k)
    while z:
        print z, v
        z, v = z(*v)
    return v



#
#(cond
#  ((a) (ae))
#  ((b) (be))
#  (else (c)))
#

# something obfuscated below is the condition itself. Since you cannot
# stick an "if" statement inside a lambda, we cheat and instead return
# one of two continuations based on the condition:
#
#   ((true_expr, false_expr)[not condition_expr])

# push_code
# load_var "a"
# push_code
# push_code
# load_var "ae"
# load_var "k"
# funcall
# pop_code
# make_fun
# push_code
# load_var "b"
# push_code
# push_code
# load_var "be"
# load_var "k"
# funcall
# pop_code
# make_fun
# push_code
# load_var "c"
# load_var "k"
# funcall
# pop_code
# make_tuple
# load_var "v"
# not
# get_nth
# funcall
# pop_code
# make_fun
# funcall
# pop_code
# make_fun
# make_tuple
# load_var "v"
# not
# get_nth
# funcall
# pop_code
# make_fun
# funcall
# popcode
# make_fun

x = lambda k: \
    a(lambda v: (lambda: ae(k),\
                 lambda: b(lambda v: (lambda: be(k), \
                                      lambda: c(k))[not v]()))
                 [not v]())


print funcall(x)


#
# The end.
