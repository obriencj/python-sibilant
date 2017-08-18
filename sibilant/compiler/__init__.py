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


import dis
import operator as pyop

from abc import ABCMeta, abstractmethod, abstractstaticmethod
from contextlib import contextmanager
from enum import Enum
from functools import partial, reduce, wraps
from io import StringIO, IOBase
from itertools import count
from platform import python_implementation
from sys import version_info
from types import CodeType

from .. import (
    SibilantException,
    symbol, is_symbol, is_keyword,
    cons, cdr, is_pair, is_proper, nil, is_nil,
)

from ..parse import (
    SibilantSyntaxError,
    default_reader, source_str, source_stream,
)


__all__ = (
    "SpecialSyntaxError", "UnsupportedVersion",
    "Opcode", "Pseudop",
    "CodeSpace", "ExpressionCodeSpace",
    "code_space_for_version",
    "Special", "is_special",
    "Macro", "is_macro",
    "Macrolet", "is_macrolet",
    "Operator", "is_operator",
    "iter_compile",
)


_symbol_attr = symbol("attr")


class CompilerSyntaxError(SibilantSyntaxError):
    pass


class UnsupportedVersion(SibilantException):
    pass


class Compiled(metaclass=ABCMeta):
    __slots__ = ("__name__", )
    __objname__ = "sibilant-compiled"


    def __init__(self, name):
        self.__name__ = name


    def __call__(self, *args, **kwds):
        msg = "Attempt to call %r as a runtime function" % self
        raise TypeError(msg)


    def __repr__(self):
        return "<%s %s>" % (self.__objname__, self.__name__)


    @abstractmethod
    def compile(self, compiler, source_obj):
        pass


def is_compiled(obj):
    return isinstance(obj, Compiled)


class Special(Compiled, metaclass=ABCMeta):
    __objname__ = "special-form"


    def __init__(self, name, compilefn):
        super().__init__(name)


    def __new__(cls, name, compilefn):
        nom = str(name or compilefn.__name__)
        mbs = {
            "__doc__": compilefn.__doc__,
            "__inline__": staticmethod(compilefn),
        }
        cls = type(nom, (cls, ), mbs)
        return object.__new__(cls)


    @abstractstaticmethod
    def __inline__(env, source_obj):
        pass


    def compile(self, compiler, source_obj):
        return self.__inline__(compiler, source_obj)


def is_special(obj):
    return isinstance(obj, Special)


class Macro(Compiled, metaclass=ABCMeta):
    """
    A Macro is defined at run-time but consumed at compile-time to
    transform a source expression. It is an error to invoke it as
    a callable at run-time.

    Call the `expand` method with a full source expression to obtain
    the one-time transformed result.
    """

    __objname__ = "macro"


    def __init__(self, name, macrofn):
        super().__init__(name)


    def __new__(cls, name, expandfn):
        nom = str(name or expandfn.__name__)
        mbs = {
            "__doc__": expandfn.__doc__,
            "expand": staticmethod(expandfn),
        }
        cls = type(nom, (cls, ), mbs)
        return object.__new__(cls)


    @abstractstaticmethod
    def expand(*args):
        pass


    def compile(self, compiler, source_obj):
        called_by, source = source_obj
        expr = self.expand(*source.unpack())

        # a Macro should always evaluate to some kind of non-None. If
        # all the work of the macro was performed in the environment
        # for some reason, it's still expected to provide an expanded
        # result, or stack underruns are almost guaranteed. Therefore
        # if the wrapped macro function returns None we will pretend
        # it expanded to the None symbol instead.
        return _symbol_None if expr is None else expr


def is_macro(obj):
    return isinstance(obj, Macro)


class Macrolet(Macro):

    def __init__(self, name, macrofn):
        super().__init__(name)


    def compile(self, compiler, source_obj):
        expanded = self.expand()
        expanded = _symbol_None if expanded is None else expanded

        if is_symbol(source_obj):
            return expanded

        elif is_pair(source_obj):
            called_by, source = source_obj
            return cons(expanded, source)

        else:
            msg = "Error expanding macrolet %s from %r" % \
                  (self.__name__, source_obj)
            compiler.error(msg, source_obj)


def is_macrolet(obj):
    return isinstance(obj, Macrolet)


class Operator(Compiled):
    __objname__ = "operator"


    def __init__(self, name, compilefn, runtimefn):
        super().__init__(name)


    def __new__(cls, name, compilefn, runtimefn):
        assert(compilefn is not None)
        assert(runtimefn is not None)
        nom = str(name or runtimefn.__name__ or compilefn.__name__)
        mbs = {
            "__doc__": compilefn.__doc__,
            "__call__": staticmethod(runtimefn),
            "compile": staticmethod(compilefn),
        }
        cls = type(nom, (cls, ), mbs)
        return object.__new__(cls)


def is_operator(obj):
    return isinstance(obj, Operator)


class Opcode(Enum):

    def hasconst(self):
        return self.value in dis.hasconst

    def hasfree(self):
        return self.value in dis.hasfree

    def hasjabs(self):
        return self.value in dis.hasjabs

    def hasjrel(self):
        return self.value in dis.hasjrel

    def haslocal(self):
        return self.value in dis.haslocal

    def hasname(self):
        return self.value in dis.hasname

    def hasnargs(self):
        return self.value in dis.hasnargs


Opcode = Opcode("Opcode", dis.opmap)


_auto = partial(next, count())


class Pseudop(Enum):
    POP = _auto()
    DUP = _auto()
    ROT_TWO = _auto()
    ROT_THREE = _auto()
    RAISE = _auto()
    CALL = _auto()
    CALL_VARARGS = _auto()
    CONST = _auto()
    GET_VAR = _auto()
    SET_VAR = _auto()
    DELETE_VAR = _auto()
    GET_ATTR = _auto()
    SET_ATTR = _auto()
    LAMBDA = _auto()
    RET_VAL = _auto()
    GET_GLOBAL = _auto()
    DEFINE_GLOBAL = _auto()
    DEFINE_LOCAL = _auto()
    JUMP = _auto()
    JUMP_FORWARD = _auto()
    POP_JUMP_IF_TRUE = _auto()
    POP_JUMP_IF_FALSE = _auto()
    COMPARE_OP = _auto()
    UNARY_POSITIVE = _auto()
    UNARY_NEGATIVE = _auto()
    UNARY_NOT = _auto()
    UNARY_INVERT = _auto()
    ITER = _auto()
    ITEM = _auto()
    BINARY_POWER = _auto()
    BINARY_MULTIPLY = _auto()
    BINARY_MATRIX_MULTIPLY = _auto()
    BINARY_FLOOR_DIVIDE = _auto()
    BINARY_TRUE_DIVIDE = _auto()
    BINARY_MODULO = _auto()
    BINARY_ADD = _auto()
    BINARY_SUBTRACT = _auto()
    BINARY_SUBSCR = _auto()
    BINARY_LSHIFT = _auto()
    BINARY_RSHIFT = _auto()
    BINARY_AND = _auto()
    BINARY_XOR = _auto()
    BINARY_OR = _auto()
    BUILD_TUPLE = _auto()
    BUILD_TUPLE_UNPACK = _auto()
    SETUP_WITH = _auto()
    WITH_CLEANUP_START = _auto()
    WITH_CLEANUP_FINISH = _auto()
    SETUP_LOOP = _auto()
    SETUP_EXCEPT = _auto()
    SETUP_FINALLY = _auto()
    END_FINALLY = _auto()
    POP_BLOCK = _auto()
    POP_EXCEPT = _auto()
    POSITION = _auto()
    LABEL = _auto()
    FAUX_PUSH = _auto()
    DEBUG_STACK = _auto()


class CodeFlag(Enum):
    OPTIMIZED = 1
    NEWLOCALS = 2
    VARARGS = 4
    VARKEYWORDS = 8
    NESTED = 16
    GENERATOR = 32
    NOFREE = 64
    COROUTINE = 128
    ITERABLE_COROUTINE = 256


# these types alone are valid constant value types when marshalling a
# code object. Outside of marshalling, Python doesn't seem to care
# what you put in the consts tuple of a code object.
_CONST_TYPES = (
    CodeType,
    str, bytes,
    tuple, list, dict, set,
    bool, int, float, complex,
    type(None), type(...),
)


_symbol_nil = symbol("nil")
_symbol_None = symbol("None")
_symbol_True = symbol("True")
_symbol_False = symbol("False")
_symbol_ellipsis = symbol("...")


def _label_generator(formatstr="label_%04i"):
    counter = 0

    def gen_label():
        nonlocal counter
        counter += 1
        return formatstr % counter

    return gen_label


class CodeSpace(metaclass=ABCMeta):
    """
    Represents a lexical scope, expressions occurring within that
    scope, and nested sub-scopes.
    """

    def __init__(self, args=(), kwargs=None, varargs=False,
                 parent=None, name=None,
                 filename=None, positions=None, declared_at=None):

        self.env = None
        self.parent = parent
        self.name = name

        self.filename = filename
        self.positions = {} if positions is None else positions
        self.declared_at = declared_at

        # vars which are only ours
        self.fast_vars = []

        # vars we have been loaned, and might re-loan to children
        self.free_vars = []

        # our own vars which we will loan to children
        self.cell_vars = []

        # global vars get stored in names as well, but this helps us
        # differentiate between global values and object member
        # accessors
        self.global_vars = []

        self.args = []
        for arg in args:
            n = str(arg)
            _list_unique_append(self.args, n)
            _list_unique_append(self.fast_vars, n)

        self.kwargs = kwargs

        self.varargs = varargs

        self.names = []

        # first const is required -- it'll be None or a doc string and
        # then None
        self.consts = [None]

        self.pseudops = []

        self.gen_label = _label_generator()
        self._gen_sym = _label_generator("gensym_" + str(id(self)) + "_%04i")

        if varargs:
            self._prep_varargs()


    def gen_sym(self):
        while True:
            sym = self._gen_sym()
            if sym in self.args or \
               sym in self.fast_vars or \
               sym in self.free_vars or \
               sym in self.cell_vars or \
               sym in self.global_vars:

                continue
            else:
                return sym


    def set_doc(self, docstr):
        consts = self.consts

        if docstr is None and self.consts[0] is not None:
            self.consts.pop(0)

        elif isinstance(docstr, str):
            if consts[0] is None:
                self.consts.insert(0, docstr)
            elif isinstance(consts[0], str):
                self.consts[0] = docstr


    def require_active(self):
        if self.env is None:
            raise Exception("compiler code space is not active")


    @contextmanager
    def activate(self, env):

        self.env = env

        old = env.get("__compiler__", None)
        env["__compiler__"] = self

        try:
            yield self

        finally:
            if old is None:
                del env["__compiler__"]
            else:
                env["__compiler__"] = old

            self.env = None


    def child(self, args=(), kwargs=None, varargs=False,
              name=None, declared_at=None):

        """
        Returns a child codespace
        """

        if declared_at is None:
            declared_at = self.declared_at

        cs = type(self)(parent=self,
                        args=args, kwargs=kwargs,
                        varargs=varargs,
                        name=name,
                        filename=self.filename,
                        positions=self.positions,
                        declared_at=declared_at)

        return cs


    def child_context(self, **kwargs):
        """
        Returns a context for a child codespace
        """

        self.require_active()
        cs = self.child(**kwargs)
        return cs.activate(self.env)


    def declare_const(self, value):
        assert (type(value) in _CONST_TYPES), "invalid const type %r" % value
        _list_unique_append(self.consts, value)


    def declare_var(self, name):
        name = str(name)
        if not (name in self.cell_vars or name in self.free_vars):
            _list_unique_append(self.fast_vars, name)


    def request_var(self, name):
        name = str(name)
        if (name in self.fast_vars) or \
           (name in self.free_vars) or \
           (name in self.cell_vars) or \
           (name in self.global_vars):

            # either the name is already available in this scope as a
            # load_fast, or we've already figured out whether it needs
            # to be found via a load_closure or load_global
            pass

        else:
            # we need to figure out if access to this var will be via
            # a load_closure, or load_global call

            if self.parent and self.parent.request_cell(name):
                # we asked our parent if we can get it as a closure,
                # and they said yes
                _list_unique_append(self.free_vars, name)
            else:
                _list_unique_append(self.global_vars, name)
                _list_unique_append(self.names, name)


    def request_global(self, name):
        _list_unique_append(self.global_vars, name)
        _list_unique_append(self.names, name)


    def request_cell(self, name):
        if name in self.global_vars:
            # no, we won't provide a cell for a global
            return False

        elif ((name in self.cell_vars) or
              (name in self.free_vars)):
            # yup, we can provide that cell. It's either already a
            # cell we created to give away, or a cell we ourselves
            # already inherited.
            return True

        elif name in self.fast_vars:
            # we need to convert this fast var into a cell var for our
            # child namespace to use
            # self.fast_vars.remove(name)
            _list_unique_append(self.cell_vars, name)
            return True

        elif self.parent and self.parent.request_cell(name):
            # we asked our parent and they had it, so now it's a cell
            # for them, and a free for us, and we can affirm that we
            # can provide it
            _list_unique_append(self.free_vars, name)
            return True

        else:
            # nope, there's no local in the nested namespace
            # inheritance to convert into a cell
            return False


    def request_name(self, name):
        name = str(name)
        _list_unique_append(self.names, name)


    def _prep_varargs(self):
        # initial step which will convert the pythonic varargs tuple
        # into a proper cons list

        if self.declared_at:
            self.pseudop_position(*self.declared_at)

        self.pseudop_get_var("make-proper")
        self.pseudop_get_var(self.args[-1])
        self.pseudop_call_varargs(0)
        self.pseudop_set_var(self.args[-1])


    def position_of(self, source):
        try:
            return self.positions[id(source)]
        except KeyError:
            return None


    def pseudop(self, *op_args):
        """
        Pushes a pseudo op and arguments into the code
        """
        self.pseudops.append(op_args)


    def pseudop_debug(self, *op_args):
        if False:
            self.pseudop(Pseudop.DEBUG_STACK, *op_args)


    def pseudop_getattr(self, name):
        self.request_name(name)
        self.pseudop(Pseudop.GET_ATTR, name)


    def pseudop_setattr(self, name):
        self.request_name(name)
        self.pseudop(Pseudop.SET_ATTR, name)


    def pseudop_rot_two(self):
        self.pseudop(Pseudop.ROT_TWO)


    def pseudop_rot_three(self):
        self.pseudop(Pseudop.ROT_THREE)


    def pseudop_faux_push(self, count=1):
        self.pseudop(Pseudop.FAUX_PUSH, count)


    def pseudop_faux_pop(self, count=1):
        self.pseudop(Pseudop.FAUX_PUSH, -count)


    def pseudop_position(self, line, column):
        self.pseudop(Pseudop.POSITION, line, column)


    def pseudop_position_of(self, cl):
        try:
            self.pseudop(Pseudop.POSITION, *self.positions[id(cl)])
        except KeyError:
            pass


    def pseudop_call(self, argc):
        self.pseudop(Pseudop.CALL, argc)


    def pseudop_call_varargs(self, argc):
        self.pseudop(Pseudop.CALL_VARARGS, argc)


    def pseudop_const(self, val):
        """
        Pushes a pseudo op to load a constant value
        """
        self.declare_const(val)
        self.pseudop(Pseudop.CONST, val)


    def pseudop_get_var(self, name):
        """
        Pushes a pseudo op to load a named value
        """
        self.request_var(name)
        self.pseudop(Pseudop.GET_VAR, name)


    def pseudop_set_var(self, name):
        """
        Pushes a pseudo op to assign to a named value
        """
        self.request_var(name)
        self.pseudop(Pseudop.SET_VAR, name)


    def pseudop_del_var(self, name):
        self.pseudop(Pseudop.DELETE_VAR, name)


    def pseudop_get_global(self, name):
        self.request_global(name)
        self.pseudop(Pseudop.GET_GLOBAL, name)


    def pseudop_lambda(self, code):
        """
        Pushes a pseudo op to load a lambda from code
        """
        self.declare_const(code)
        self.declare_const(code.co_name)
        self.pseudop(Pseudop.LAMBDA, code)


    def pseudop_pop(self):
        self.pseudop(Pseudop.POP)


    def pseudop_dup(self):
        self.pseudop(Pseudop.DUP)


    def pseudop_return(self):
        """
        Pushes a pseudo op to return the top of stack
        """
        self.pseudop(Pseudop.RET_VAL)


    def pseudop_return_none(self):
        """
        Pushes a pseudo op to return None
        """
        self.pseudop_const(None)
        self.pseudop(Pseudop.RET_VAL)


    def pseudop_define_global(self, name):
        """
        Pushes a pseudo op to globally define TOS to name
        """
        _list_unique_append(self.global_vars, name)
        _list_unique_append(self.names, name)
        self.pseudop(Pseudop.DEFINE_GLOBAL, name)


    def pseudop_define_local(self, name):
        """
        Pushes a pseudo op to globally define TOS to name
        """
        self.declare_var(name)
        self.pseudop(Pseudop.DEFINE_LOCAL, name)


    def pseudop_label(self, name):
        self.pseudop(Pseudop.LABEL, name)


    def pseudop_jump(self, label_name):
        self.pseudop(Pseudop.JUMP, label_name)


    def pseudop_jump_forward(self, label_name):
        self.pseudop(Pseudop.JUMP_FORWARD, label_name)


    def pseudop_pop_jump_if_true(self, label_name):
        self.pseudop(Pseudop.POP_JUMP_IF_TRUE, label_name)


    def pseudop_pop_jump_if_false(self, label_name):
        self.pseudop(Pseudop.POP_JUMP_IF_FALSE, label_name)


    def pseudop_build_tuple(self, count):
        self.pseudop(Pseudop.BUILD_TUPLE, count)


    def pseudop_build_tuple_unpack(self, count):
        self.pseudop(Pseudop.BUILD_TUPLE_UNPACK, count)


    def pseudop_setup_loop(self, done_label):
        self.pseudop(Pseudop.SETUP_LOOP, done_label)


    def pseudop_setup_with(self, try_label):
        self.pseudop(Pseudop.SETUP_WITH, try_label)


    def pseudop_with_cleanup_start(self):
        self.pseudop(Pseudop.WITH_CLEANUP_START)


    def pseudop_with_cleanup_finish(self):
        self.pseudop(Pseudop.WITH_CLEANUP_FINISH)


    def pseudop_setup_except(self, try_label):
        self.pseudop(Pseudop.SETUP_EXCEPT, try_label)


    def pseudop_setup_finally(self, final_label):
        self.pseudop(Pseudop.SETUP_FINALLY, final_label)


    def pseudop_pop_block(self):
        self.pseudop(Pseudop.POP_BLOCK)


    def pseudop_pop_except(self):
        self.pseudop(Pseudop.POP_EXCEPT)


    def pseudop_end_finally(self):
        self.pseudop(Pseudop.END_FINALLY)


    def pseudop_unary_positive(self):
        self.pseudop(Pseudop.UNARY_POSITIVE)


    def pseudop_unary_negative(self):
        self.pseudop(Pseudop.UNARY_NEGATIVE)


    def pseudop_unary_not(self):
        self.pseudop(Pseudop.UNARY_NOT)


    def pseudop_unary_invert(self):
        self.pseudop(Pseudop.UNARY_INVERT)


    def pseudop_binary_add(self):
        self.pseudop(Pseudop.BINARY_ADD)


    def pseudop_binary_subtract(self):
        self.pseudop(Pseudop.BINARY_SUBTRACT)


    def pseudop_binary_multiply(self):
        self.pseudop(Pseudop.BINARY_MULTIPLY)


    def pseudop_binary_power(self):
        self.pseudop(Pseudop.BINARY_POWER)


    def pseudop_binary_modulo(self):
        self.pseudop(Pseudop.BINARY_MODULO)


    def pseudop_binary_divide(self):
        self.pseudop(Pseudop.BINARY_TRUE_DIVIDE)


    def pseudop_binary_floor_divide(self):
        self.pseudop(Pseudop.BINARY_FLOOR_DIVIDE)


    def pseudop_iter(self):
        self.pseudop(Pseudop.ITER)


    def pseudop_item(self):
        self.pseudop(Pseudop.ITEM)


    def pseudop_compare_lt(self):
        self.pseudop(Pseudop.COMPARE_OP, 0)


    def pseudop_compare_lte(self):
        self.pseudop(Pseudop.COMPARE_OP, 1)


    def pseudop_compare_eq(self):
        self.pseudop(Pseudop.COMPARE_OP, 2)


    def pseudop_compare_not_eq(self):
        self.pseudop(Pseudop.COMPARE_OP, 3)


    def pseudop_compare_gt(self):
        self.pseudop(Pseudop.COMPARE_OP, 4)


    def pseudop_compare_gte(self):
        self.pseudop(Pseudop.COMPARE_OP, 5)


    def pseudop_compare_in(self):
        self.pseudop(Pseudop.COMPARE_OP, 6)


    def pseudop_compare_not_in(self):
        self.pseudop(Pseudop.COMPARE_OP, 7)


    def pseudop_compare_is(self):
        self.pseudop(Pseudop.COMPARE_OP, 8)


    def pseudop_compare_is_not(self):
        self.pseudop(Pseudop.COMPARE_OP, 9)


    def pseudop_compare_exception(self):
        self.pseudop(Pseudop.COMPARE_OP, 10)


    def pseudop_raise(self, count):
        self.pseudop(Pseudop.RAISE, count)
        self.pseudop(Pseudop.FAUX_PUSH, 1)


    def complete(self):
        """
        Produces a python code object representing the state of this
        CodeSpace
        """

        self.require_active()

        argcount = len(self.args)

        # this is the number of fast variables, plus the variables
        # converted to cells for child scope usage
        # nlocals = len(self.fast_vars) + len(self.cell_vars)

        stacksize = max_stack(self.pseudops)

        flags = CodeFlag.OPTIMIZED.value | CodeFlag.NEWLOCALS.value

        if self.varargs:
            argcount -= 1
            flags |= CodeFlag.VARARGS.value

        if not (self.free_vars or self.cell_vars):
            flags |= CodeFlag.NOFREE.value

        if self.parent:
            flags |= CodeFlag.NESTED.value

        lnt = []
        code = self.code_bytes(lnt)

        consts = tuple(self.consts)

        names = tuple(self.names)

        varnames = list(self.fast_vars)
        for v in self.cell_vars:
            _list_unique_append(varnames, v)
        varnames = tuple(varnames)

        nlocals = len(varnames)

        filename = "<sibilant>" if self.filename is None else self.filename

        name = "<anon>" if self.name is None else self.name

        firstlineno = self.declared_at[0] if self.declared_at else None
        firstlineno, lnotab = self.lnt_compile(lnt, firstline=firstlineno)

        freevars = tuple(self.free_vars)
        cellvars = tuple(self.cell_vars)

        ret = CodeType(argcount, 0, nlocals, stacksize, flags, code,
                       consts, names, varnames, filename, name,
                       firstlineno, lnotab, freevars, cellvars)

        if False:
            print("completed a CodeSpace", ret)
            dis.show_code(ret)
            print("Disassembly:")
            dis.dis(ret)
            print()

        return ret


    @abstractmethod
    def lnt_compile(self, line_number_table, firstline=1):
        pass


    @abstractmethod
    def code_bytes(self, line_number_table):
        pass


def code_space_for_version(ver=version_info,
                           impl=python_implementation()):
    """
    Returns the relevant SpecialCodeSpace subclass to emit bytecode
    for the relevant version of Python
    """

    if impl == 'CPython':
        if (3, 6) <= ver <= (3, 7):
            from .cpython36 import CPython36
            return CPython36

        elif (3, 5) <= ver <= (3, 6):
            from .cpython35 import CPython35
            return CPython35

    return None


class ExpressionCodeSpace(CodeSpace):
    """
    Adds special forms to the basic functionality of CodeSpace
    """


    def add_expression(self, expr):
        """
        Insert an expression into the code space. expr should be a cons
        cell representing the expression. If the expression appears to
        be a special form (either a macro defined in the CodeSpace's
        env, or a pre-defined built-in special), it will be expanded
        and compiled to pseudo ops.
        """

        self.require_active()

        self.pseudop_position_of(expr)

        while expr is not None:
            if expr is nil:
                self.pseudop_get_var("nil")
                expr = None

            elif is_pair(expr):
                expr = self.compile_pair(expr)

            elif is_symbol(expr):
                expr = self.compile_symbol(expr)

            else:
                # TODO there are some literal types that can't be used
                # as constants, will need to fill those in here. For
                # now we're just presuming it's going to be a
                # constant, the end.
                expr = self.pseudop_const(expr)


    def add_expression_with_return(self, expr):
        """
        Insert an expression, then an op to return its result from the
        current call
        """
        self.add_expression(expr)
        self.pseudop_return()


    def compile_pair(self, expr):
        head, tail = expr

        if is_symbol(head):
            # see if this is a special, either a builtin one
            # or a defined macro.
            special = self.find_compiled(head)
            if special:
                return special.compile(self, expr)

        # either not a symbol, or it was and the symbol wasn't
        # a special, so just make it into a function call
        for cl in expr.unpack():
            self.add_expression(cl)

        self.pseudop_position_of(expr)
        self.pseudop_call(expr.count() - 1)
        return None


    def compile_symbol(self, sym):
        """
        The various ways that a symbol on its own can evaluate.
        """

        # TODO: check if symbol is a macrolet and expand it

        if is_keyword(sym):
            # it should be fairly rare that a keyword is actually
            # passed anywhere at runtime -- it's mostly meant for use
            # as a marker in source expressions for specials.

            self.pseudop_get_var("keyword")
            self.pseudop_const(str(sym))
            self.pseudop_call(1)
            return None

        elif sym is _symbol_None:
            return self.pseudop_const(None)

        elif sym is _symbol_True:
            return self.pseudop_const(True)

        elif sym is _symbol_False:
            return self.pseudop_const(False)

        elif sym is _symbol_ellipsis:
            return self.pseudop_const(...)

        else:
            ex = sym.rsplit(".", 1)
            if len(ex) == 1:
                return self.pseudop_get_var(str(sym))
            else:
                return cons(_symbol_attr, *ex, nil)


    def error(self, message, source):
        return CompilerSyntaxError(message, self.position_of(source),
                                   filename=self.filename)


    def find_compiled(self, namesym):
        self.require_active()

        # okay, let's look through the environment by name
        name = str(namesym)
        env = self.env

        try:
            # is it in globals?
            found = env[name]
        except KeyError:
            try:
                # nope, how about in builtins?
                env = env["__builtins__"].__dict__
                found = env[name]

            except KeyError:
                # nope
                found = None

        if found and is_compiled(found):
            # we found a Macro instance, return the relevant
            # method
            return found

        else:
            # we either found nothing, or what we found doesn't
            # qualify
            return None


def _list_unique_append(onto_list, value):
    # we have to manually loop and use the `is` operator, because the
    # list.index method will match False with 0 and True with 1, which
    # incorrectly collapses consts pools when both values are present

    for index, found in enumerate(onto_list):
        if found is value:
            return index
    else:
        onto_list.append(value)
        return len(onto_list)


# def op_max_stack(opargs):
#     depth = 0
#
#    for op, args in opargs:
#        effect = op.stack_effect(*args)
#        depth += effect
#        if depth > maxdepth:
#            maxdepth = depth
#
#        if op.hasjrel or op.hasjabs:
#            pass


# def label_graph(pseudops):
#     jump_points = {}

#     for op, args in pseudops:
#         if op is Pseudop.LABEL:
#             labal = args[0]
#             point = jump_points.get(label)
#             if point is None:
#                 point = []
#                 jump_points[label] = point

#             current_point = point


def max_stack(pseudops):
    """
    Calculates the maximum stack size from the pseudo operations. This
    function is total crap, but it's good enough for now.
    """

    # save ourselves one bazillion global lookups of the same value
    pseu = Pseudop

    maxc = 0
    stac = 0
    at_label = {}

    def push(by=1):
        nonlocal maxc, stac
        stac += by
        if stac > maxc:
            maxc = stac

    def pop(by=1):
        nonlocal stac
        stac -= by
        if stac < 0:
            print("SHIT BROKE")
            print(pseudops)
        assert (stac >= 0), "max_stack counter underrun"

    # print("max_stack()")
    for op, *args in pseudops:
        # print(op, args, stac, maxc)

        if op is pseu.POSITION:
            pass

        elif op is pseu.DEBUG_STACK:
            print(" ".join(map(str, args)), "max:", maxc, "current:", stac)

        elif op is pseu.CALL:
            pop(args[0])

        elif op is pseu.CONST:
            push()

        elif op in (pseu.GET_VAR,
                    pseu.GET_GLOBAL):
            push()

        elif op is pseu.SET_VAR:
            pop()

        elif op is pseu.DELETE_VAR:
            pass

        elif op in (pseu.GET_ATTR,
                    pseu.UNARY_POSITIVE,
                    pseu.UNARY_NEGATIVE,
                    pseu.UNARY_NOT,
                    pseu.UNARY_INVERT,
                    pseu.ITER):
            pop()
            push()

        elif op is pseu.SET_ATTR:
            pop(2)

        elif op is pseu.DUP:
            push()

        elif op in (pseu.DEFINE_GLOBAL,
                    pseu.DEFINE_LOCAL):
            pop()

        elif op is pseu.POP:
            pop()

        elif op is pseu.LAMBDA:
            a = len(args[0].co_freevars)
            if a:
                push(a)
                pop(a)
            push(2)
            pop(2)
            push()

        elif op is pseu.RET_VAL:
            pop()

        elif op in (pseu.JUMP,
                    pseu.JUMP_FORWARD):
            at_label[args[0]] = stac

        elif op is pseu.LABEL:
            stac = at_label.get(args[0], stac)

        elif op in (pseu.POP_JUMP_IF_TRUE,
                    pseu.POP_JUMP_IF_FALSE):
            pop()
            at_label[args[0]] = stac

        elif op is pseu.CALL_VARARGS:
            # TODO: need to revamp CALL_VARARGS to act on an optional
            # kwargs as well
            pop()

        elif op in (pseu.BUILD_TUPLE,
                    pseu.BUILD_TUPLE_UNPACK):
            pop(args[0])
            push()

        elif op in (pseu.SETUP_EXCEPT,
                    pseu.SETUP_WITH,
                    pseu.SETUP_FINALLY):
            push(4)

        elif op in (pseu.POP_BLOCK,
                    pseu.POP_EXCEPT):

            pop(4)

        elif op is pseu.WITH_CLEANUP_START:
            push(4)

        elif op is pseu.WITH_CLEANUP_FINISH:
            pop(4)

        elif op is pseu.END_FINALLY:
            pop(1)

        elif op in (pseu.COMPARE_OP,
                    pseu.ITEM,
                    pseu.BINARY_ADD,
                    pseu.BINARY_SUBTRACT,
                    pseu.BINARY_MULTIPLY,
                    pseu.BINARY_TRUE_DIVIDE,
                    pseu.BINARY_FLOOR_DIVIDE,
                    pseu.BINARY_POWER,
                    pseu.BINARY_MODULO):
            pop(2)
            push()

        elif op is pseu.RAISE:
            pop(args[0])

        elif op is pseu.FAUX_PUSH:
            push(args[0])

        elif op in (pseu.ROT_THREE,
                    pseu.ROT_TWO):
            pass

        else:
            assert False, "unknown pseudop %r" % op

    assert (stac == 0), "%i left-over stack items" % stac
    return maxc


def iter_compile(source, env, filename=None, reader=None):

    if isinstance(source, str):
        source = source_str(source)

    elif isinstance(source, IOBase):
        source = source_stream(source)

    if reader is None:
        reader = default_reader

    factory = code_space_for_version(version_info)
    if not factory:
        raise UnsupportedVersion(version_info)

    positions = source.positions

    env["__stream__"] = source
    env["__reader__"] = reader
    env["read"] = reader.read

    while True:
        codespace = factory(filename=filename, positions=positions)
        with codespace.activate(env):
            assert(env.get("__compiler__") == codespace)

            # read until EOF
            expr = reader.read(source)
            if expr is None:
                break

            # compile
            codespace.add_expression_with_return(expr)
            code = codespace.complete()
        yield code


#
# The end.
