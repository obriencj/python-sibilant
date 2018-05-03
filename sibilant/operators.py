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
The built-in operators with compile-time optimizations.

Operators are sibilant forms which compile directly to bytedoce, and
can also be used at runtime to produce an equivalent function.

author: Christopher O'Brien <obriencj@gmail.com>
license: LGPL v.3
"""


from .compiler import Operator

from .lib import (
    symbol, nil, is_pair,
    build_tuple, build_list, build_set, build_dict
)

from functools import reduce

import operator as pyop
from operator import (
    __add__, __sub__, __mul__, __truediv__, __floordiv__,
)


__all__ = []


_symbol_add = symbol("add")
_symbol_add_ = symbol("+")
_symbol_and = symbol("and")
_symbol_bit_and = symbol("bitwise-and")
_symbol_bit_and_ = symbol("&")
_symbol_bit_or = symbol("bitwise-or")
_symbol_bit_or_ = symbol("|")
_symbol_bit_xor = symbol("bitwise-xor")
_symbol_bit_xor_ = symbol("^")
_symbol_build_dict = symbol("build-dict")
_symbol_build_list = symbol("build-list")
_symbol_build_set = symbol("build-set")
_symbol_build_slice = symbol("build-slice")
_symbol_build_str = symbol("build-str")
_symbol_build_tuple = symbol("build-tuple")
_symbol_contains = symbol("contains")
_symbol_del_item = symbol("del-item")
_symbol_div = symbol("divide")
_symbol_div_ = symbol("/")
_symbol_eq = symbol("eq")
_symbol_eq_ = symbol("==")
_symbol_floordiv = symbol("floor-divide")
_symbol_floordiv_ = symbol("//")
_symbol_format = symbol("format")
_symbol_ge = symbol("ge")
_symbol_ge_ = symbol(">=")
_symbol_gt = symbol("gt")
_symbol_gt_ = symbol(">")
_symbol_hash_dict = symbol("#dict")
_symbol_hash_list = symbol("#list")
_symbol_hash_set = symbol("#set")
_symbol_hash_slice = symbol("#slice")
_symbol_hash_str = symbol("#str")
_symbol_hash_tuple = symbol("#tuple")
_symbol_in = symbol("in")
_symbol_invert = symbol("~")
_symbol_is = symbol("is")
_symbol_is_not = symbol("is-not")
_symbol_item = symbol("item")
_symbol_iter = symbol("iter")
_symbol_le = symbol("le")
_symbol_le_ = symbol("<=")
_symbol_lshift = symbol("shift-left")
_symbol_lshift_ = symbol("<<")
_symbol_lt = symbol("lt")
_symbol_lt_ = symbol("<")
_symbol_matrix_mult = symbol("matrix-multiply")
_symbol_matrix_mult_ = symbol("@")
_symbol_mod = symbol("modulo")
_symbol_mod_ = symbol("%")
_symbol_mult = symbol("multiply")
_symbol_mult_ = symbol("*")
_symbol_nil = symbol("nil")
_symbol_not = symbol("not")
_symbol_not_contains = symbol("not-contains")
_symbol_not_eq = symbol("not-eq")
_symbol_not_eq_ = symbol("!=")
_symbol_not_in = symbol("not-in")
_symbol_or = symbol("or")
_symbol_pow = symbol("power")
_symbol_pow_ = symbol("**")
_symbol_raise = symbol("raise")
_symbol_rshift = symbol("shift-right")
_symbol_rshift_ = symbol(">>")
_symbol_set_item = symbol("set-item")
_symbol_slice = symbol("slice")
_symbol_sub = symbol("subtract")
_symbol_sub_ = symbol("-")


def operator(namesym, runtime, *aliases):
    name = str(namesym)

    # runtime = partial(runtime)
    # runtime.__name__ = name

    glbls = globals()

    def deco(compilefn):
        compilefn.__name__ = name
        inst = Operator(name, compilefn, runtime)

        __all__.append(name)
        glbls[name] = inst

        for alias in aliases:
            alias = str(alias)
            __all__.append(alias)
            glbls[alias] = inst

        return compilefn

    return deco


# --- conditionally reducing operators ---


def runtime_and(*vals):
    val = True
    for val in vals:
        if not val:
            break
    return val


@operator(_symbol_and, runtime_and)
def operator_and(code, source, tc=False):
    """
    (and EXPR...)
    Evaluates expressions in order until one returns a false-ish
    result, then returns it. Otherwise, returns the last value.
    """

    called_by, exprs = source

    code.pseudop_position_of(source)
    code.pseudop_const(True)

    end_label = code.gen_label()
    while exprs:
        code.pseudop_pop()

        ex, exprs = exprs
        code.add_expression(ex)
        code.pseudop_dup()
        code.pseudop_pop_jump_if_false(end_label)

    code.pseudop_label(end_label)

    return None


def runtime_or(*vals):
    val = False
    for val in vals:
        if val:
            break
    return val


@operator(_symbol_or, runtime=runtime_or)
def operator_or(code, source, tc=False):
    """
    (or EXPR...)
    Evaluates expressions in order until one returns a true-ish
    result, then returns it. Otherwise, returns the last value.
    """

    called_by, exprs = source

    code.pseudop_position_of(source)
    code.pseudop_const(False)

    end_label = code.gen_label()
    while exprs:
        code.pseudop_pop()

        ex, exprs = exprs
        code.add_expression(ex)
        code.pseudop_dup()
        code.pseudop_pop_jump_if_true(end_label)

    code.pseudop_label(end_label)

    return None


# --- reducing operators ---


def runtime_add(val, *vals):
    return reduce(__add__, vals, val) if vals else +val


@operator(_symbol_add, runtime_add, _symbol_add_)
def operator_add(code, source, tc=False):
    """
    (+ VAL)
    applies unary_positive to VAL

    (+ VAL VAL...)
    adds the first two values together. Then adds the result to the
    next value. Continues until only the result remains.
    """

    called_by, rest = source
    if rest is nil:
        raise code.error("too few arguments to %s" % called_by, source)

    code.pseudop_position_of(source)

    val, rest = rest
    code.add_expression(val)

    if rest is nil:
        code.pseudop_unary_positive()

    else:
        while rest:
            val, rest = rest
            code.add_expression(val)
            code.pseudop_binary_add()

    return None


def runtime_subtract(val, *vals):
    return reduce(__sub__, vals, val) if vals else -val


@operator(_symbol_sub, runtime_subtract, _symbol_sub_)
def operator_subtract(code, source, tc=False):
    """
    (- VAL)
    applies unary_negative to VAL

    (- VAL VAL...)
    subtracts the second value from the first value. Then
    subtracts the next value from the result. Continues until only
    the result remains.
    """

    called_by, rest = source
    if rest is nil:
        raise code.error("too few arguments to %s" % called_by, source)

    code.pseudop_position_of(source)

    val, rest = rest
    code.add_expression(val)

    if rest is nil:
        code.pseudop_unary_negative()

    else:
        while rest:
            val, rest = rest
            code.add_expression(val)
            code.pseudop_binary_subtract()

    return None


def runtime_multiply(val, *vals):
    return reduce(__mul__, vals, val) if vals else (1 * val)


@operator(_symbol_mult, runtime_multiply, _symbol_mult_)
def operator_multiply(code, source, tc=False):
    """
    (* VAL)
    same as (* 1 VAL)

    (* VAL VAL...)
    multiplies values together, from left to right.
    """

    called_by, rest = source
    if rest is nil:
        raise code.error("too few arguments to %s" % called_by, source)

    code.pseudop_position_of(source)

    val, rest = rest
    if rest is nil:
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


def runtime_divide(val, *vals):
    return reduce(__truediv__, vals, val) if vals else (1 / val)


@operator(_symbol_div, runtime_divide, _symbol_div_)
def operator_divide(code, source, tc=False):
    """
    (/ VAL)
    same as (/ 1 VAL)

    (/ VAL VAL...)
    divides the first value from the second, and then divides the
    result by the next value
    """

    called_by, rest = source
    if rest is nil:
        raise code.error("too few arguments to %s" % called_by, source)

    code.pseudop_position_of(source)

    val, rest = rest
    if rest is nil:
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


def runtime_floor_divide(val, *vals):
    return reduce(__floordiv__, vals, val) if vals else (1 // val)


@operator(_symbol_floordiv, runtime_floor_divide, _symbol_floordiv_)
def operator_floor_divide(code, source, tc=False):
    """
    (// VAL)
    same as (// 1 VAL)

    (// VAL VAL...)
    divides the first value from the second, and then divides the
    result by the next value
    """

    called_by, rest = source
    if rest is nil:
        raise code.error("too few arguments to %s" % called_by, source)

    code.pseudop_position_of(source)

    val, rest = rest
    if rest is nil:
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


# --- ternary operators ---


def _helper_ternary(code, source, opfun):
    name, rest = source

    try:
        left, (middle, (right, rest)) = rest

    except ValueError:
        raise code.error("too few arguments to %s" % name, source)

    if rest:
        raise code.error("too many arguments to %s" % name, source)

    code.pseudop_position_of(source)

    code.add_expression(right)
    code.add_expression(left)
    code.add_expression(middle)

    code.pseudop_position_of(source)
    opfun()


@operator(_symbol_set_item, pyop.setitem)
def operator_set_item(code, source, tc=False):
    """
    (set-item OBJ KEY VALUE)

    Evaluates OBJ KEY and VALUE in order. Assign's the KEY'th
    index of OBJ to VALUE
    """

    _helper_ternary(code, source, code.pseudop_set_item)
    code.pseudop_const(None)


# --- binary operators ---


def _helper_binary(code, source, opfun, flip=False):
    name, rest = source

    try:
        left, (right, rest) = rest

    except ValueError:
        raise code.error("too few arguments to %s" % name, source)

    if rest:
        raise code.error("too many arguments to %s" % name, source)

    code.pseudop_position_of(source)

    if flip:
        code.add_expression(right)
        code.add_expression(left)

    else:
        code.add_expression(left)
        code.add_expression(right)

    code.pseudop_position_of(source)
    opfun()


@operator(_symbol_item, pyop.getitem)
def operator_item(code, source, tc=False):
    """
    (item OBJ KEY)
    gets item from OBJ by key KEY
    """

    _helper_binary(code, source, code.pseudop_get_item)


@operator(_symbol_del_item, pyop.delitem)
def operator_del_item(code, source, tc=False):
    """
    (del-item OBJ KEY)
    gets item from OBJ by KEY
    """

    _helper_binary(code, source, code.pseudop_del_item)
    code.pseudop_const(None)


@operator(_symbol_pow, pyop.pow, _symbol_pow_)
def operator_power(code, source, tc=False):
    """
    (** VAL EXPONENT)
    raises VAL to the EXPONENT
    """

    _helper_binary(code, source, code.pseudop_binary_power)


@operator(_symbol_mod, pyop.mod, _symbol_mod_)
def operator_modulo(code, source, tc=False):
    """
    (% VAL MOD)
    VAL modulo MOD. If VAL is a string, Pythonic string
    substitution is invoked.
    """

    _helper_binary(code, source, code.pseudop_binary_modulo)


@operator(_symbol_matrix_mult, pyop.matmul, _symbol_matrix_mult_)
def operator_matmul(code, source, tc=False):
    """
    (matrix-multiply MATRIX MATRIX)
    Multiply two matrices, return the result
    """

    _helper_binary(code, source, code.pseudop_binary_matrix_multiply)


@operator(_symbol_lshift, pyop.lshift, _symbol_lshift_)
def operator_lshift(code, source, tc=False):
    """
    (<< VALUE COUNT)
    Bitshift VALUE left by COUNT bits
    """

    _helper_binary(code, source, code.pseudop_binary_lshift)


@operator(_symbol_rshift, pyop.rshift, _symbol_rshift_)
def operator_rshift(code, source, tc=False):
    """
    (>> VALUE COUNT)
    Bitshift VALUE right by COUNT bits
    """

    _helper_binary(code, source, code.pseudop_binary_rshift)


@operator(_symbol_bit_and, pyop.and_, _symbol_bit_and_)
def operator_bit_and(code, source, tc=False):
    """
    (& VALUE MASK)
    Applies bitwise-and MASK to VALUE

    (bitwise-and VALUE MASK)
    same as above
    """

    _helper_binary(code, source, code.pseudop_binary_and)


@operator(_symbol_bit_or, pyop.or_, _symbol_bit_or_)
def operator_bit_or(code, source, tc=False):
    """
    (| VALUE SETMASK)
    Applies bitwise-or SETMASK to VALUE

    (bitwise-or VALUE SETMASK)
    same as above
    """

    _helper_binary(code, source, code.pseudop_binary_or)


@operator(_symbol_bit_xor, pyop.xor, _symbol_bit_xor_)
def operator_bit_xor(code, source, tc=False):
    """
    (^ VALUE FLIPMASK)
    Applies bitwise-xor FLIPMASK to VALUE

    (bitwise-xor VALUE FLIPMASK)
    same as above
    """

    _helper_binary(code, source, code.pseudop_binary_xor)


@operator(_symbol_gt, pyop.gt, _symbol_gt_)
def operator_gt(code, source, tc=False):
    """
    (> VAL1 VAL2)
    True if VAL1 is greater-than VAL2

    (gt VAL1 VAL2)
    same as above
    """

    _helper_binary(code, source, code.pseudop_compare_gt)


@operator(_symbol_ge, pyop.ge, _symbol_ge_)
def operator_ge(code, source, tc=False):
    """
    (>= VAL1 VAL2)
    True if VAL1 is greater-than or equal-to VAL2

    (ge VAL1 VAL2)
    same as above
    """

    _helper_binary(code, source, code.pseudop_compare_gte)


@operator(_symbol_contains, pyop.contains)
def operator_contains(code, source, tc=False):
    """
    (contains SEQUENCE VALUE)
    True if SEQUENCE contains VALUE

    Identical to (in VALUE SEQUENCE)
    """

    _helper_binary(code, source, code.pseudop_compare_in, True)


@operator(_symbol_in, (lambda value, seq: value in seq))
def operator_in(code, source, tc=False):
    """
    (in VALUE SEQUENCE)
    True if SEQUENCE contains VALUE

    Identical to (contains SEQUENCE VALUE)
    """

    _helper_binary(code, source, code.pseudop_compare_in)


@operator(_symbol_not_contains, (lambda seq, value: value not in seq))
def operator_not_contains(code, source, tc=False):
    """
    (not-contains SEQUENCE VALUE)
    False if SEQUENCE contains VALUE

    Identical to (not-in VALUE SEQUENCE)
    """

    _helper_binary(code, source, code.pseudop_compare_not_in, True)


@operator(_symbol_not_in, (lambda value, seq: value not in seq))
def operator_not_in(code, source, tc=False):
    """
    (not-in VALUE SEQUENCE)
    False if SEQUENCE contains VALUE

    Identical to (not-contains SEQUENCE VALUE)
    """

    _helper_binary(code, source, code.pseudop_compare_not_in)


@operator(_symbol_is, pyop.is_)
def operator_is(code, source, tc=False):
    """
    (is OBJ1 OBJ2)
    True if OBJ1 and OBJ2 are the same object
    """

    _helper_binary(code, source, code.pseudop_compare_is)


@operator(_symbol_is_not, pyop.is_not)
def operator_is_not(code, source, tc=False):
    """
    (is-not OBJ1 OBJ2)
    True if OBJ1 and OBJ2 are different objects
    """

    _helper_binary(code, source, code.pseudop_compare_is_not)


@operator(_symbol_lt, pyop.lt, _symbol_lt_)
def operator_lt(code, source, tc=False):
    """
    (< VAL1 VAL2)
    True if VAL1 is less-than VAL2

    (lt VAL1 VAL2)
    same as above
    """
    _helper_binary(code, source, code.pseudop_compare_lt)


@operator(_symbol_le, pyop.le, _symbol_le_)
def operator_le(code, source, tc=False):
    """
    (<= VAL1 VAL2)
    True if VAL1 is less-than or equal-to VAL2

    (le VAL1 VAL2)
    same as above
    """

    _helper_binary(code, source, code.pseudop_compare_lte)


@operator(_symbol_eq, pyop.eq, _symbol_eq_)
def operator_eq(code, source, tc=False):
    """
    (== VAL1 VAL2)
    True if VAL1 and VAL2 are equal

    (eq VAL1 VAL2)
    same as above
    """

    _helper_binary(code, source, code.pseudop_compare_eq)


@operator(_symbol_not_eq, pyop.ne, _symbol_not_eq_)
def operator_not_eq(code, source, tc=False):
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

    if rest:
        raise code.error("too many arguments to %s" % name, source)

    code.pseudop_position_of(source)
    code.add_expression(expr)

    code.pseudop_position_of(source)
    opfun()


@operator(_symbol_not, pyop.not_)
def operator_not(code, source, tc=False):
    """
    (not VAL)
    Boolean inversion of VAL. If VAL is true-like, returns
    False. Otherwise, True
    """

    _helper_unary(code, source, code.pseudop_unary_not)


@operator(_symbol_invert, pyop.invert)
def operator_invert(code, source, tc=False):
    """
    (~ VAL)
    Binary inversion of VAL
    """

    _helper_unary(code, source, code.pseudop_unary_invert)


@operator(_symbol_iter, iter)
def operator_iter(code, source, tc=False):
    """
    (iter OBJ)
    Produces an iterator over the contents of OBJ
    """

    _helper_unary(code, source, code.pseudop_iter)


def runtime_raise(exc=None):
    if exc is None:
        raise
    else:
        raise exc


@operator(_symbol_raise, runtime_raise)
def operator_raise(code, source, tc=False):
    """
    (raise EXCEPTION_EXPR)

    evaluates EXCEPTION_EXPR and raises it in the Python interpreter.
    This function does not return, and execution will jump to whatever
    outer exception handlers exist (if any).

    (raise)

    re-raises the most recently raised exception
    """

    called_by, cl = source

    c = cl.length()
    if c > 3:
        msg = "too many arguments to raise %r" % cl
        raise code.error(msg, source)

    for rx in cl.unpack():
        code.add_expression(rx)

    code.pseudop_position_of(source)
    code.pseudop_raise(c)

    return None


@operator(_symbol_build_tuple, build_tuple, _symbol_hash_tuple)
def operator_build_tuple(code, source, tc=False):
    """
    (build-tuple ITEM...)

    evaluates ITEM expressions in order, and results in a tuple of
    the results.
    """

    called_by, items = source

    c = 0
    for c, ce in enumerate(items.unpack(), 1):
        code.add_expression(ce)

    code.pseudop_position_of(source)
    code.pseudop_build_tuple(c)

    return None


@operator(_symbol_build_list, build_list, _symbol_hash_list)
def operator_build_list(code, source, tc=False):
    """
    (build-list ITEM...)

    evaluates ITEM expressions in order, and results in a list of
    the results.
    """

    called_by, items = source

    c = 0
    for c, ce in enumerate(items.unpack(), 1):
        code.add_expression(ce)

    code.pseudop_position_of(source)
    code.pseudop_build_list(c)

    return None


@operator(_symbol_build_set, build_set, _symbol_hash_set)
def operator_build_set(code, source, tc=False):
    """
    (build-set ITEM...)

    evaluates ITEM expressions in order, and results in a set of
    the results.
    """

    called_by, items = source

    c = 0
    for c, ce in enumerate(items.unpack(), 1):
        code.add_expression(ce)

    code.pseudop_position_of(source)
    code.pseudop_build_set(c)

    return None


@operator(_symbol_build_dict, build_dict, _symbol_hash_dict)
def operator_build_dict(code, source, tc=False):
    """
    (build-dict (KEY VAL)...)

    Produces a dictionary instance where each KEY to a VAL. KEY
    and VAL are evaluated in order of appearance.
    """

    called_by, items = source

    c = 0
    for c, ce in enumerate(items.unpack(), 1):
        if is_pair(ce):
            cu = list(ce.unpack())
            if len(cu) != 2:
                msg = "too many elements in build-dict item #%i" % c
                raise code.error(msg, source)
            else:
                code.add_expression(cu[0])
                code.add_expression(cu[1])
        else:
            code.add_expression(ce)
            code.pseudop_unpack_sequence(2)

    code.pseudop_position_of(source)
    code.pseudop_build_map(c)

    return None


def runtime_build_str(*subs):
    return "".join(subs) if subs else ""


def collapse_build_str(seq):
    tmp = list()

    for part in seq:
        if type(part) is str:
            if part:
                tmp.append(part)
        else:
            if tmp:
                yield "".join(tmp)
                tmp = []
            if part:
                yield part

    if tmp:
        yield "".join(tmp)


@operator(_symbol_build_str, runtime_build_str, _symbol_hash_str)
def operator_build_str(code, source, tc=False):
    """
    (build-str VAL...)

    Concatenates string values together
    """

    called_by, items = source

    if items is nil:
        code.pseudop_const("")
        return None

    # first collapse neighboring string literals together.
    parts = list(collapse_build_str(items.unpack()))
    for part in parts:
        code.add_expression(part)

    lp = len(parts)
    if lp > 1:
        code.pseudop_build_str(lp)
    elif lp == 1:
        pass
    else:
        code.pseudop_const("")

    return None


@operator(_symbol_build_slice, slice, _symbol_hash_slice)
def operator_build_slice(code, source, tc=False):
    """
    (build-slice START STOP)
    (build-slice START STOP STEP)

    Create a slice object.
    """

    try:
        called_by, (start, (stop, rest)) = source
    except ValueError:
        raise code.error("too few arguments to slice", source)

    if rest:
        step, rest = rest
    else:
        step = None

    if rest:
        raise code.error("too many arguments to slice", source)

    code.add_expression(start)
    code.add_expression(stop)

    if step is not None:
        code.add_expression(step)
        code.pseudop_build_slice(3)
    else:
        code.pseudop_build_slice(2)

    return None


@operator(_symbol_format, format)
def operator_format(code, source, tc=False):
    """
    (format VALUE)
    (format VALUE SPEC)

    Formats value using the formatting mini-language
    """

    try:
        called_by, (val, rest) = source
    except ValueError:
        raise code.error("too few arguments to format", source)

    if rest:
        spec, rest = rest
    else:
        spec = None

    if rest:
        raise code.error("too many arguments to format", source)

    code.add_expression(val)
    if spec:
        code.add_expression(spec)
        code.pseudop_format(0x04)
    else:
        code.pseudop_format(0x00)

    return None


#
# The end.
