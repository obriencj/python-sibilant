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
The built-in operators with compile-time optimizations
"""


import operator as pyop

from functools import reduce

from .. import symbol, is_nil


__all__ = []


_symbol_nil = symbol("nil")

_symbol_item = symbol("item")
_symbol_add = symbol("add")
_symbol_add_ = symbol("+")
_symbol_sub = symbol("subtract")
_symbol_sub_ = symbol("-")
_symbol_mult = symbol("multiply")
_symbol_mult_ = symbol("*")
_symbol_matrix_mult = symbol("matrix-multiply")
_symbol_matrix_mult_ = symbol("@")
_symbol_pow = symbol("power")
_symbol_pow_ = symbol("**")
_symbol_mod = symbol("modulo")
_symbol_mod_ = symbol("%")
_symbol_div = symbol("divide")
_symbol_div_ = symbol("/")
_symbol_floordiv = symbol("floor-divide")
_symbol_floordiv_ = symbol("//")
_symbol_lshift = symbol("shift-left")
_symbol_lshift_ = symbol("<<")
_symbol_rshift = symbol("shift-right")
_symbol_rshift_ = symbol(">>")
_symbol_bit_and = symbol("bitwise-and")
_symbol_bit_and_ = symbol("&")
_symbol_bit_or = symbol("bitwise-or")
_symbol_bit_or_ = symbol("|")
_symbol_bit_xor = symbol("bitwise-xor")
_symbol_bit_xor_ = symbol("^")

_symbol_lt = symbol("lt")
_symbol_lt_ = symbol("<")
_symbol_le = symbol("le")
_symbol_le_ = symbol("<=")
_symbol_eq = symbol("eq")
_symbol_eq_ = symbol("==")
_symbol_not_eq = symbol("not-eq")
_symbol_not_eq_ = symbol("!=")
_symbol_gt = symbol("gt")
_symbol_gt_ = symbol(">")
_symbol_ge = symbol("ge")
_symbol_ge_ = symbol(">=")
_symbol_in = symbol("in")
_symbol_not_in = symbol("not-in")
_symbol_is = symbol("is")
_symbol_is_not = symbol("is-not")

_symbol_and = symbol("and")
_symbol_or = symbol("or")
_symbol_not = symbol("not")
_symbol_invert = symbol("~")
_symbol_iter = symbol("iter")

_symbol_raise = symbol("raise")


def operator():
    from . import Operator
    glbls = globals()

    def operator(namesym, runtime, *aliases):
        name = str(namesym)

        # runtime = partial(runtime)
        # runtime.__name__ = name

        def deco(compilefn):
            compilefn.__name__ = name
            inst = Operator(name, compilefn, runtime)

            __all__.append(name)
            glbls[name] = inst

            for alias in aliases:
                alias = str(alias)
                __all__.append(alias)
                glbls[alias] = inst

            return inst

        return deco

    return operator

operator = operator()


# --- conditionally reducing operators ---


def _runtime_and(*vals):
    val = True
    for val in vals:
        if not val:
            break
    return val


def _helper_and(code, exprs):
    code.pseudop_const(True)

    end_label = code.gen_label()
    while exprs:
        code.pseudop_pop()

        ex, exprs = exprs
        code.add_expression(ex)
        code.pseudop_dup()
        code.pseudop_pop_jump_if_false(end_label)

    code.pseudop_label(end_label)


@operator(_symbol_and, _runtime_and)
def _operator_and(code, source, tc=False):
    """
    (and EXPR...)
    Evaluates expressions in order until one returns a false-ish
    result, then returns it. Otherwise, returns the last value.
    """

    called_by, rest = source

    code.pseudop_position_of(source)
    _helper_and(code, rest)

    return None


def _runtime_or(*vals):
    val = False
    for val in vals:
        if val:
            break
    return val


def _helper_or(code, exprs):
    code.pseudop_const(False)

    end_label = code.gen_label()
    while exprs:
        code.pseudop_pop()

        ex, exprs = exprs
        code.add_expression(ex)
        code.pseudop_dup()
        code.pseudop_pop_jump_if_true(end_label)

    code.pseudop_label(end_label)


@operator(_symbol_or, runtime=_runtime_or)
def _operator_or(code, source, tc=False):
    """
    (or EXPR...)
    Evaluates expressions in order until one returns a true-ish
    result, then returns it. Otherwise, returns the last value.
    """

    called_by, rest = source

    code.pseudop_position_of(source)
    _helper_or(code, rest)

    return None


# --- reducing operators ---


def _runtime_add(val, *vals):
    if vals:
        return reduce(pyop.add, vals, val)
    else:
        return +val


@operator(_symbol_add, _runtime_add, _symbol_add_)
def _operator_add(code, source, tc=False):
    """
    (+ VAL)
    applies unary_positive to VAL

    (+ VAL VAL...)
    adds the first two values together. Then adds the result to the
    next value. Continues until only the result remains.
    """

    called_by, rest = source
    if is_nil(rest):
        raise code.error("too few arguments to %s" % called_by, source)

    code.pseudop_position_of(source)

    val, rest = rest
    code.add_expression(val)

    if is_nil(rest):
        code.pseudop_unary_positive()

    else:
        while rest:
            val, rest = rest
            code.add_expression(val)
            code.pseudop_binary_add()

    return None


def _runtime_subtract(val, *vals):
    if vals:
        return reduce(pyop.sub, vals, val)
    else:
        return -val


@operator(_symbol_sub, _runtime_subtract, _symbol_sub_)
def _operator_subtract(code, source, tc=False):
    """
    (- VAL)
    applies unary_negative to VAL

    (- VAL VAL...)
    subtracts the second value from the first value. Then
    subtracts the next value from the result. Continues until only
    the result remains.
    """

    called_by, rest = source
    if is_nil(rest):
        raise code.error("too few arguments to %s" % called_by, source)

    code.pseudop_position_of(source)

    val, rest = rest
    code.add_expression(val)

    if is_nil(rest):
        code.pseudop_unary_negative()

    else:
        while rest:
            val, rest = rest
            code.add_expression(val)
            code.pseudop_binary_subtract()

    return None


def _runtime_multiply(val, *vals):
    if vals:
        return reduce(pyop.mul, vals, val)
    else:
        return 1 * val


@operator(_symbol_mult, _runtime_multiply, _symbol_mult_)
def _operator_multiply(code, source, tc=False):
    """
    (* VAL)
    same as (* 1 VAL)

    (* VAL VAL...)
    multiplies values together, from left to right.
    """

    called_by, rest = source
    if is_nil(rest):
        raise code.error("too few arguments to %s" % called_by, source)

    code.pseudop_position_of(source)

    val, rest = rest
    if is_nil(rest):
        code.pseudop_const(1)
        code.add_expression(val)
        code.pseudop_binary_multiply()

    else:
        code.add_expression(val)
        while rest:
            val, rest = rest
            code.add_expression(val)
            code.pseudop_binary_multiply()

    return None


def _runtime_divide(val, *vals):
    if vals:
        return reduce(pyop.truediv, vals, val)
    else:
        return 1 / val


@operator(_symbol_div, _runtime_divide, _symbol_div_)
def _operator_divide(code, source, tc=False):
    """
    (/ VAL)
    same as (/ 1 VAL)

    (/ VAL VAL...)
    divides the first value from the second, and then divides the
    result by the next value
    """

    called_by, rest = source
    if is_nil(rest):
        raise code.error("too few arguments to %s" % called_by, source)

    code.pseudop_position_of(source)

    val, rest = rest
    if is_nil(rest):
        code.pseudop_const(1)
        code.add_expression(val)
        code.pseudop_binary_divide()

    else:
        code.add_expression(val)
        while rest:
            val, rest = rest
            code.add_expression(val)
            code.pseudop_binary_divide()

    return None


def _runtime_floor_divide(val, *vals):
    if vals:
        return reduce(pyop.floordiv, vals, val)
    else:
        return 1 // val


@operator(_symbol_floordiv, _runtime_floor_divide, _symbol_floordiv_)
def _operator_floor_divide(code, source, tc=False):
    """
    (// VAL)
    same as (// 1 VAL)

    (// VAL VAL...)
    divides the first value from the second, and then divides the
    result by the next value
    """

    called_by, rest = source
    if is_nil(rest):
        raise code.error("too few arguments to %s" % called_by, source)

    code.pseudop_position_of(source)

    val, rest = rest
    if is_nil(rest):
        code.pseudop_const(1)
        code.add_expression(val)
        code.pseudop_binary_floor_divide()

    else:
        code.add_expression(val)
        while rest:
            val, rest = rest
            code.add_expression(val)
            code.pseudop_binary_floor_divide()

    return None


# --- binary operators ---


def _helper_binary(code, source, opfun, flip=False):
    name, rest = source

    try:
        left, (right, rest) = rest

    except ValueError:
        raise code.error("too few arguments to %s" % name, source)

    if not is_nil(rest):
        raise code.error("too many arguments to %s" % name, source)

    code.pseudop_position_of(source)

    if flip:
        code.add_expression(right)
        code.add_expression(left)

    else:
        code.add_expression(left)
        code.add_expression(right)

    opfun()


@operator(_symbol_item, pyop.getitem)
def _operator_item(code, source, tc=False):
    """
    (item OBJ KEY)
    gets item from OBJ by key KEY
    """

    _helper_binary(code, source, code.pseudop_item)


@operator(_symbol_pow, pyop.pow, _symbol_pow_)
def _operator_power(code, source, tc=False):
    """
    (** VAL EXPONENT)
    raises VAL to the EXPONENT
    """

    _helper_binary(code, source, code.pseudop_binary_power)


@operator(_symbol_mod, pyop.mod, _symbol_mod_)
def _operator_modulo(code, source, tc=False):
    """
    (% VAL MOD)
    VAL modulo MOD. If VAL is a string, Pythonic string
    substitution is invoked.
    """

    _helper_binary(code, source, code.pseudop_binary_modulo)


@operator(_symbol_matrix_mult, pyop.matmul, _symbol_matrix_mult_)
def _operator_matmul(code, source, tc=False):
    """
    (matrix-multiply MATRIX MATRIX)
    Multiply two matrices, return the result
    """

    _helper_binary(code, source, code.pseudop_binary_matrix_multiply)


@operator(_symbol_lshift, pyop.lshift, _symbol_lshift_)
def _operator_lshift(code, source, tc=False):
    """
    (<< VALUE COUNT)
    Bitshift VALUE left by COUNT bits
    """

    _helper_binary(code, source, code.pseudop_binary_lshift)


@operator(_symbol_rshift, pyop.rshift, _symbol_rshift_)
def _operator_rshift(code, source, tc=False):
    """
    (>> VALUE COUNT)
    Bitshift VALUE right by COUNT bits
    """

    _helper_binary(code, source, code.pseudop_binary_rshift)


@operator(_symbol_bit_and, pyop.and_, _symbol_bit_and_)
def _operator_bit_and(code, source, tc=False):
    """
    (& VALUE MASK)
    Applies bitwise-and MASK to VALUE

    (bitwise-and VALUE MASK)
    same as above
    """

    _helper_binary(code, source, code.pseudop_binary_and)


@operator(_symbol_bit_or, pyop.or_, _symbol_bit_or_)
def _operator_bit_or(code, source, tc=False):
    """
    (| VALUE SETMASK)
    Applies bitwise-or SETMASK to VALUE

    (bitwise-or VALUE SETMASK)
    same as above
    """

    _helper_binary(code, source, code.pseudop_binary_or)


@operator(_symbol_bit_xor, pyop.xor, _symbol_bit_xor_)
def _operator_bit_xor(code, source, tc=False):
    """
    (^ VALUE FLIPMASK)
    Applies bitwise-xor FLIPMASK to VALUE

    (bitwise-xor VALUE FLIPMASK)
    same as above
    """

    _helper_binary(code, source, code.pseudop_binary_xor)


@operator(_symbol_gt, pyop.gt, _symbol_gt_)
def _operator_gt(code, source, tc=False):
    """
    (> VAL1 VAL2)
    True if VAL1 is greater-than VAL2

    (gt VAL1 VAL2)
    same as above
    """

    _helper_binary(code, source, code.pseudop_compare_gt)


@operator(_symbol_ge, pyop.ge, _symbol_ge_)
def _operator_ge(code, source, tc=False):
    """
    (>= VAL1 VAL2)
    True if VAL1 is greater-than or equal-to VAL2

    (ge VAL1 VAL2)
    same as above
    """

    _helper_binary(code, source, code.pseudop_compare_gte)


@operator(_symbol_in, pyop.contains)
def _operator_in(code, source, tc=False):
    """
    (in SEQ VALUE)
    True if SEQ contains VALUE
    """

    _helper_binary(code, source, code.pseudop_compare_in, True)


@operator(_symbol_not_in, (lambda seq, value: value not in seq))
def _operator_not_in(code, source, tc=False):
    """
    (not-in SEQ VALUE)
    False if SEQ contains VALUE
    """

    _helper_binary(code, source, code.pseudop_compare_not_in, True)


@operator(_symbol_is, pyop.is_)
def _operator_is(code, source, tc=False):
    """
    (is OBJ1 OBJ2)
    True if OBJ1 and OBJ2 are the same object
    """

    _helper_binary(code, source, code.pseudop_compare_is)


@operator(_symbol_is_not, pyop.is_not)
def _operator_is_not(code, source, tc=False):
    """
    (is-not OBJ1 OBJ2)
    True if OBJ1 and OBJ2 are different objects
    """

    _helper_binary(code, source, code.pseudop_compare_is_not)


@operator(_symbol_lt, pyop.lt, _symbol_lt_)
def _operator_lt(code, source, tc=False):
    """
    (< VAL1 VAL2)
    True if VAL1 is less-than VAL2

    (lt VAL1 VAL2)
    same as above
    """
    _helper_binary(code, source, code.pseudop_compare_lt)


@operator(_symbol_le, pyop.le, _symbol_le_)
def _operator_le(code, source, tc=False):
    """
    (<= VAL1 VAL2)
    True if VAL1 is less-than or equal-to VAL2

    (le VAL1 VAL2)
    same as above
    """

    _helper_binary(code, source, code.pseudop_compare_lte)


@operator(_symbol_eq, pyop.eq, _symbol_eq_)
def _operator_eq(code, source, tc=False):
    """
    (== VAL1 VAL2)
    True if VAL1 and VAL2 are equal

    (eq VAL1 VAL2)
    same as above
    """

    _helper_binary(code, source, code.pseudop_compare_eq)


@operator(_symbol_not_eq, pyop.ne, _symbol_not_eq_)
def _operator_not_eq(code, source, tc=False):
    """
    (!= VAL1 VAL2)
    True if VAL1 and VAL2 are not equal

    (ne VAL1 VAL2)
    same as above
    """

    _helper_binary(code, source, code.pseudop_compare_not_eq)


# --- unary operators ---


def _helper_unary(code, source, opfun):
    try:
        name, (expr, rest) = source
    except ValueError:
        raise code.error("too few arguments to %s" % name, source)

    if not is_nil(rest):
        raise code.error("too many arguments to %s" % name, source)

    code.pseudop_position_of(source)
    code.add_expression(expr)
    opfun()


@operator(_symbol_not, pyop.not_)
def _operator_not(code, source, tc=False):
    """
    (not VAL)
    Boolean inversion of VAL. If VAL is true-like, returns
    False. Otherwise, True
    """

    _helper_unary(code, source, code.pseudop_unary_not)


@operator(_symbol_invert, pyop.invert)
def _operator_invert(code, source, tc=False):
    """
    (~ VAL)
    Binary inversion of VAL
    """

    _helper_unary(code, source, code.pseudop_unary_invert)


@operator(_symbol_iter, iter)
def _operator_iter(code, source, tc=False):
    """
    (iter OBJ)
    Produces an iterator over the contents of OBJ
    """

    _helper_unary(code, source, code.pseudop_iter)


def _runtime_raise(exc=None):
    if exc:
        raise exc
    else:
        raise


@operator(_symbol_raise, _runtime_raise)
def _special_raise(code, source, tc=False):
    """
    (raise EXCEPTION_EXPR)

    evaluates EXCEPTION_EXPR and raises it in the Python interpreter.
    This function does not return, and execution will jump to whatever
    outer exception handlers exist (if any).

    (raise)

    re-raises the most recently raised exception
    """

    called_by, cl = source

    c = cl.count()
    if c > 3:
        msg = "too many arguments to raise %r" % cl
        raise code.error(msg, source)

    for rx in cl.unpack():
        code.add_expression(rx)

    code.pseudop_position_of(source)
    code.pseudop_raise(c)

    return None


# --- and finally clean up ---


__all__ = tuple(__all__)


#
# The end.
