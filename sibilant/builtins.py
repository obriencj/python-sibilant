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
builtin definitions for sibilant. These are all following k-style
conventions.

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


import operator
import sibilant

from functools import partial, reduce


def _reduce_op(opf, name=None):

    def fun(*a):
        return reduce(opf, a)

    name = name if name else opf.__name__
    sym = sibilant.symbol(name)

    fun.__name__ = name
    fun.__doc__ = opf.__doc__
    fun.__symbol__ = sym

    globals()[sym] = fun
    globals()[str(sym)] = fun

    return fun


def _op(opf, name=None):
    fun = partial(opf)

    name = name if name else opf.__name__
    sym = sibilant.symbol(name)

    fun.__name__ = name
    fun.__doc__ = opf.__doc__
    fun.__symbol__ = sym
    globals()[sym] = fun
    globals()[str(sym)] = fun

    return fun


def _val(value, name):
    globals()[sibilant.symbol(name)] = value
    globals()[str(name)] = value
    return value


__add__ = _reduce_op(operator.add, "+")
__sub__ = _reduce_op(operator.sub, "-")
__mul__ = _reduce_op(operator.mul, "*")
__pow__ = _op(operator.pow, "**")
__div__ = _op(operator.truediv, "/")
__mod__ = _op(operator.mod, "%")
__floordiv__ = _op(operator.floordiv, "//")
__bitwise_or__ = _op(operator.or_, "|")
__bitwise_and__ = _op(operator.and_, "&")
__bitwise_xor__ = _op(operator.xor, "^")
__bitwise_invert__ = _op(operator.invert, "~")

__cons = _op(sibilant.cons)
__car = _op(sibilant.car)
__cdr = _op(sibilant.cdr)
__ref = _op(sibilant.ref)
__attr = _op(sibilant.attr)
__deref = _op(sibilant.deref)
__setref = _op(sibilant.setref)

__symbol = _val(sibilant.symbol, "symbol")
__nil = _val(sibilant.nil, "nil")
__constype = _val(sibilant.constype, "constype")
__niltype  = _val(sibilant.niltype, "niltype")
__reftype = _val(sibilant.reftype, "reftype")
__attrtype = _val(sibilant.attrtype, "attrtype")
__undefined = _val(sibilant.undefined, "undefined")

__builtins_sym = __symbol("__builtins__")

__quote_sym = __symbol("__builtins__.quote")
__unquote_sym = __symbol("__builtins__.unquote")
__quasiquote_sym = __symbol("__builtins__.quasiquote")
__splice_sym = __symbol("__builtins__.splice")


def is_special(f):
    return hasattr(f, "special")


def _eval(v, env):
    print("_eval: ", v)

    if isinstance(v, __symbol):
        return env[v] if v in env else env[__builtins_sym][v]

    elif isinstance(v, __constype):
        s = __car(v)
        if s is __quote_sym:
            return __cdr(v)
        elif s is __quasiquote_sym:
            pass
        else:
            f = _eval(__car(v), env)
            if is_special(f):
                return f.special(v, env)
            else:
                a = (_eval(i, env) for i in __cdr(v).unpack())
                return f(*a)
    else:
        return v


__eval = _op(_eval, "eval")


def _prep_env(**base):

    gl = globals()

    base.update({__symbol(k): v for k, v in base.items()})

    base["__builtins__"] = gl
    base[__builtins_sym] = gl

    # {k: v for k, v in gl.items() if type(k) is __symbol}

    return base


#
# The end.
