#! /usr/bin/env python3

def ev(sym):
    def lookup(k):
        print("fetching", sym)
        k(sym)
    return lookup


def co(exp, k):
    return lambda _: exp(lambda x: k(_+[x]))

def la(exp, k):
    return lambda _: exp(lambda x: k(x))

# useful for quoted list, and apply
# list(foo bar baz)
(lambda fin: co(ev("foo"), co(ev("bar"), co(ev("baz"), fin)))(list()))(print)

# useful for begin/progn and lambda body
# last(foo bar baz)
(lambda fin: la(ev("foo"), la(ev("bar"), la(ev("baz"), fin)))(None))(print)

# list inside a list
co(ev("foo"), co(ev("bar"), co(ev("baz"), print)))(list())

# TODO:
# make co *EMIT* the lambda


def co_s(exp, k):
    return "".join(("(lambda _: (", exp,
                    ")(lambda __: (", k,
                    ")(_+[__])))"))


def ev_s(sym):
    return "".join(("(lambda _: _(", repr(sym), "))"))


def collect_k(exprs, k):
    if exprs:
        return co_s(ev_s(exprs[0]), collect_k(exprs[1:], k))
    else:
        return k


def collect_k_list(exprs):
    return "".join(("(lambda _: lambda __: _(list())(__))",
                    collect_k(exprs, "(lambda _: lambda __: __(_))")))


src = (collect_k_list(["foo"]))
eva = eval(src)
print(src)
print(eva)
eva(print)

src = (collect_k_list(["foo", "bar", "baz"]))
eva = eval(src)
print(src)
print(eva)
eva(print)
