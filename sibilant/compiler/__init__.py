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
    "CodeSpace", "SpecialCodeSpace",
    "code_space_for_version",
    "Special", "is_special",
    "Macro", "is_special",
    "Operator", "is_operator",
    "iter_compile",
)


class SpecialSyntaxError(SibilantSyntaxError):
    pass


class UnsupportedVersion(SibilantException):
    pass


class Builtin(object):
    __slots__ = ("__name__", )
    __objname__ = "sibilant-builtin"

    def __init__(self, name):
        self.__name__ = name

    def __repr__(self):
        return "<%s %s>" % (self.__objname__, self.__name__)


class Runtime(Builtin, metaclass=ABCMeta):
    __objname__ = "sibilant-runtime"


    def __call__(self, *args, **kwds):
        return self.__runtime__(*args, **kwds)


    @abstractstaticmethod
    def __runtime__(*args, **kwds):
        pass


class Compiled(Builtin, metaclass=ABCMeta):
    __objname__ = "sibilant-compiled"


    def __call__(self, *args, **kwds):
        msg = "Attempt to call %r as a runtime function" % self
        raise TypeError(msg)


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


    def compile(self, _compiler, source_obj):
        called_by, source = source_obj
        return self.expand(*source.unpack())


def is_macro(obj):
    return isinstance(obj, Macro)


class Operator(Runtime, Compiled, metaclass=ABCMeta):
    __objname__ = "operator"


    def __init__(self, name, compilefn, runtimefn):
        self.__name__ = name


    def __new__(cls, name, compilefn, runtimefn):
        assert(compilefn is not None)
        assert(runtimefn is not None)
        nom = str(name or runtimefn.__name__ or compilefn.__name__)
        mbs = {
            "__doc__": compilefn.__doc__,
            "__runtime__": staticmethod(runtimefn),
            "__inline__": staticmethod(compilefn),
        }
        cls = type(nom, (cls, ), mbs)
        return object.__new__(cls)


    def compile(self, compiler, source_obj):
        return self.__inline__(compiler, source_obj)


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
_symbol_doc = symbol("doc")
_symbol_attr = symbol("attr")
_symbol_set_attr = symbol("set-attr")
_symbol_setq = symbol("setq")
_symbol_global = symbol("global")
_symbol_define = symbol("define")
_symbol_define_global = symbol("define-global")
_symbol_define_local = symbol("define-local")
_symbol_defmacro = symbol("defmacro")
_symbol_quote = symbol("quote")
_symbol_quasiquote = symbol("quasiquote")
_symbol_unquote = symbol("unquote")
_symbol_splice = symbol("unquote-splicing")
_symbol_begin = symbol("begin")
_symbol_cond = symbol("cond")
_symbol_lambda = symbol("lambda")
_symbol_function = symbol("function")
_symbol_with = symbol("with")
_symbol_let = symbol("let")
_symbol_while = symbol("while")
_symbol_raise = symbol("raise")
_symbol_try = symbol("try")
_symbol_else = symbol("else")
_symbol_finally = symbol("finally")

_symbol_and = symbol("and")
_symbol_or = symbol("or")
_symbol_not = symbol("not")
_symbol_invert = symbol("~")
_symbol_iter = symbol("iter")

_symbol_item = symbol("item")
_symbol_add = symbol("+")
_symbol_add_ = symbol("add")
_symbol_sub = symbol("-")
_symbol_sub_ = symbol("subtract")
_symbol_mult = symbol("*")
_symbol_mult_ = symbol("multiply")
_symbol_pow = symbol("**")
_symbol_pow_ = symbol("pow")
_symbol_mod = symbol("%")
_symbol_mod_ = symbol("mod")
_symbol_div = symbol("/")
_symbol_div_ = symbol("divide")
_symbol_floordiv = symbol("//")
_symbol_floordiv_ = symbol("floor-divide")

_symbol_lt = symbol("<")
_symbol_lt_ = symbol("lt")
_symbol_lte = symbol("<=")
_symbol_lte_ = symbol("lte")
_symbol_eq = symbol("==")
_symbol_eq_ = symbol("eq")
_symbol_not_eq = symbol("!=")
_symbol_not_eq_ = symbol("not-eq")
_symbol_gt = symbol(">")
_symbol_gt_ = symbol("gt")
_symbol_gte = symbol(">=")
_symbol_gte_ = symbol("gte")
_symbol_in = symbol("in")
_symbol_not_in = symbol("not-in")
_symbol_is = symbol("is")
_symbol_is_not = symbol("is-not")

_symbol_None = symbol("None")
_symbol_True = symbol("True")
_symbol_False = symbol("False")
_symbol_ellipsis = symbol("...")


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

        self.gen_label = label_generator()
        self._gen_sym = label_generator("gensym_" + str(id(self)) + "_%04i")

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


    # def helper_debug(self, text):
    #     # injects a print
    #     if False:
    #         self.pseudop_get_var("print")
    #         self.pseudop_const(text)
    #         self.pseudop_call(1)
    #         self.pseudop_pop()


    def helper_keyword(self, kwd):
        """
        Pushes a the pseudo ops necessary to put a keyword on the stack
        """
        self.pseudop_get_var("keyword")
        self.pseudop_const(str(kwd))
        self.pseudop_call(1)


    def helper_symbol(self, sym):
        """
        Pushes a the pseudo ops necessary to put a symbol on the stack
        """
        self.pseudop_get_var("symbol")
        self.pseudop_const(str(sym))
        self.pseudop_call(1)


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
        # print("consts:", repr(consts))

        names = tuple(self.names)

        varnames = list(self.fast_vars)
        for v in self.cell_vars:
            _list_unique_append(varnames, v)
        varnames = tuple(varnames)

        nlocals = len(varnames)
        # varnames = *self.fast_vars, *self.cell_vars

        filename = "<sibilant>" if self.filename is None else self.filename
        name = "<anon>" if self.name is None else self.name

        firstlineno = self.declared_at[0] if self.declared_at else None
        firstlineno, lnotab = lnt_compile(lnt, firstline=firstlineno)

        # print("lnotab is:", repr(lnotab))

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
    def code_bytes(self, line_number_table):
        pass


def _special():
    _specials = {}

    def special(namesym, *aliases):
        def deco(meth):
            spec = Special(str(namesym), meth)

            _specials[namesym] = spec
            for alias in aliases:
                _specials[alias] = spec

            return meth

        return deco

    def operator(namesym, runtime, *aliases):
        def deco(meth):
            spec = Operator(str(namesym), meth, runtime)

            _specials[namesym] = spec
            for alias in aliases:
                _specials[alias] = spec

            return meth

        return deco

    return special, operator, _specials.items


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


def _runtime_and(*vals):
    res = True
    for val in vals:
        res = res and val
        if not res:
            break
    return res


def _runtime_or(*vals):
    res = False
    for val in vals:
        res = res or val
        if res:
            break
    return res


def _runtime_add(val, *vals):
    if vals:
        return reduce(pyop.add, vals, val)
    else:
        return +val


def _runtime_subtract(val, *vals):
    if vals:
        return reduce(pyop.sub, vals, val)
    else:
        return -val


def _runtime_multiply(val, *vals):
    if vals:
        return reduce(pyop.mul, vals, val)
    else:
        return 1 * val


def _runtime_divide(val, *vals):
    if vals:
        return reduce(pyop.truediv, vals, val)
    else:
        return 1 / val


def _runtime_floor_divide(val, *vals):
    if vals:
        return reduce(pyop.floordiv, vals, val)
    else:
        return 1 // val


class SpecialCodeSpace(CodeSpace):
    """
    Adds special forms to the basic functionality of CodeSpace
    """

    # decorator and lookup function for built-in special forms
    special, operator, all_specials = _special()


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

        while True:
            if expr is nil:
                return self.pseudop_get_var("nil")

            elif is_pair(expr):
                orig = expr
                head, tail = expr

                if is_symbol(head):
                    # see if this is a special, either a builtin one
                    # or a defined macro.
                    special = self.find_compiled(head)
                    if special:
                        expr = special.compile(self, expr)
                        if expr is None:
                            # the special form or macro has done all
                            # the work already (injecting pseudo ops,
                            # etc), and no further transformations on
                            # the expression are needed.
                            return

                        else:
                            # we've expanded a macro or special form,
                            # so we need to start over on the
                            # resulting transformed expression.
                            continue

                # either not a symbol, or it was and the symbol wasn't
                # a special, so just make it into a function call
                for cl in expr.unpack():
                    self.add_expression(cl)

                self.pseudop_position_of(orig)
                self.pseudop_call(expr.count() - 1)
                return None

            elif is_symbol(expr):
                expr = self.compile_symbol(expr)
                if expr is None:
                    return
                else:
                    continue

            else:
                # TODO there are some literal types that can't be used
                # as constants, will need to fill those in here. For
                # now we're just presuming it's going to be a
                # constant, the end.
                return self.pseudop_const(expr)


    def add_expression_with_return(self, expr):
        """
        Insert an expression, then an op to return its result from the
        current call
        """
        self.add_expression(expr)
        self.pseudop_return()


    def compile_symbol(self, sym):
        """
        The various ways that a symbol on its own can evaluate.
        """

        # TODO: check if symbol is a macrolet and expand it

        if is_keyword(sym):
            return self.helper_keyword(str(sym))
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
        return SpecialSyntaxError(message, self.position_of(source),
                                  filename=self.filename)


    @special(_symbol_doc)
    def special_doc(self, source):
        called_by, rest = source

        self.set_doc(" ".join(d.strip() for d in map(str, rest.unpack())))

        # doc special expression evaluates to None
        self.pseudop_const(None)

        return None


    @operator(_symbol_and, _runtime_and)
    def operator_and(self, source):
        """
        (and EXPR...)
        Evaluates expressions in order until one returns a false-ish
        result, then returns it. Otherwise, returns the last value.
        """

        called_by, rest = source

        self.pseudop_position_of(source)
        self.helper_and(rest)

        return None


    def helper_and(self, exprs):
        self.pseudop_const(True)

        end_label = self.gen_label()
        while exprs:
            self.pseudop_pop()

            ex, exprs = exprs
            self.add_expression(ex)
            self.pseudop_dup()
            self.pseudop_pop_jump_if_false(end_label)

        self.pseudop_label(end_label)


    @operator(_symbol_or, runtime=_runtime_or)
    def operator_or(self, source):
        """
        (or EXPR...)
        Evaluates expressions in order until one returns a true-ish
        result, then returns it. Otherwise, returns the last value.
        """

        called_by, rest = source

        self.pseudop_position_of(source)
        self.helper_or(rest)

        return None


    def helper_or(self, exprs):
        self.pseudop_const(False)

        end_label = self.gen_label()
        while exprs:
            self.pseudop_pop()

            ex, exprs = exprs
            self.add_expression(ex)
            self.pseudop_dup()
            self.pseudop_pop_jump_if_true(end_label)

        self.pseudop_label(end_label)


    @operator(_symbol_item, pyop.getitem)
    def operator_item(self, source):
        """
        (item OBJ KEY)
        gets item from OBJ by key KEY
        """

        self._helper_binary(source, self.pseudop_item)


    @operator(_symbol_add, _runtime_add, _symbol_add_)
    def operator_add(self, source):
        """
        (+ VAL)
        applies unary_positive to VAL

        (+ VAL VAL...)
        adds the first two values together. Then adds the result to the
        next value. Continues until only the result remains.
        """

        called_by, rest = source
        if is_nil(rest):
            self.error("too few arguments to %s" % called_by, source)

        self.pseudop_position_of(source)

        val, rest = rest
        self.add_expression(val)

        if is_nil(rest):
            self.pseudop_unary_positive()

        else:
            while rest:
                val, rest = rest
                self.add_expression(val)
                self.pseudop_binary_add()

        return None


    @operator(_symbol_sub, _runtime_subtract, _symbol_sub_)
    def operator_subtract(self, source):
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
            self.error("too few arguments to %s" % called_by, source)

        self.pseudop_position_of(source)

        val, rest = rest
        self.add_expression(val)

        if is_nil(rest):
            self.pseudop_unary_negative()

        else:
            while rest:
                val, rest = rest
                self.add_expression(val)
                self.pseudop_binary_subtract()

        return None


    @operator(_symbol_mult, _runtime_multiply, _symbol_mult_)
    def operator_multiply(self, source):
        """
        (* VAL)
        same as (* 1 VAL)

        (* VAL VAL...)
        multiplies values together, from left to right.
        """

        called_by, rest = source
        if is_nil(rest):
            self.error("too few arguments to %s" % called_by, source)

        self.pseudop_position_of(source)

        val, rest = rest
        if is_nil(rest):
            self.pseudop_const(1)
            self.add_expression(val)
            self.pseudop_binary_multiply()

        else:
            self.add_expression(val)
            while rest:
                val, rest = rest
                self.add_expression(val)
                self.pseudop_binary_multiply()

        return None


    @operator(_symbol_div, _runtime_divide, _symbol_div_)
    def operator_divide(self, source):
        """
        (/ VAL)
        same as (/ 1 VAL)

        (/ VAL VAL...)
        divides the first value from the second, and then divides the
        result by the next value
        """

        called_by, rest = source
        if is_nil(rest):
            self.error("too few arguments to %s" % called_by, source)

        self.pseudop_position_of(source)

        val, rest = rest
        if is_nil(rest):
            self.pseudop_const(1)
            self.add_expression(val)
            self.pseudop_binary_divide()

        else:
            self.add_expression(val)
            while rest:
                val, rest = rest
                self.add_expression(val)
                self.pseudop_binary_divide()

        return None


    @operator(_symbol_floordiv, _runtime_floor_divide, _symbol_floordiv_)
    def operator_floor_divide(self, source):
        """
        (// VAL)
        same as (// 1 VAL)

        (// VAL VAL...)
        divides the first value from the second, and then divides the
        result by the next value
        """

        called_by, rest = source
        if is_nil(rest):
            self.error("too few arguments to %s" % called_by, source)

        self.pseudop_position_of(source)

        val, rest = rest
        if is_nil(rest):
            self.pseudop_const(1)
            self.add_expression(val)
            self.pseudop_binary_floor_divide()

        else:
            self.add_expression(val)
            while rest:
                val, rest = rest
                self.add_expression(val)
                self.pseudop_binary_floor_divide()

        return None


    @operator(_symbol_pow, pyop.pow, _symbol_pow_)
    def operator_power(self, source):
        """
        (** VAL EXPONENT)
        raises VAL to the EXPONENT
        """

        self._helper_binary(source, self.pseudop_binary_power)


    @operator(_symbol_mod, pyop.mod, _symbol_mod_)
    def operator_modulo(self, source):
        """
        (% VAL MOD)
        VAL modulo MOD. If VAL is a string, Pythonic string
        substitution is invoked.
        """

        self._helper_binary(source, self.pseudop_binary_modulo)


    @operator(_symbol_not, pyop.not_)
    def operator_not(self, source):
        try:
            called_by, (expr, rest) = source
        except ValueError:
            raise self.error("too few arguments to not", source)

        if not is_nil(rest):
            raise self.error("too many arguments to not", source)

        self.pseudop_position_of(source)

        self.add_expression(expr)
        self.pseudop_unary_not()


    @operator(_symbol_invert, pyop.invert)
    def operator_invert(self, source):
        try:
            called_by, (expr, rest) = source
        except ValueError:
            raise self.error("too few arguments to invert", source)

        if not is_nil(rest):
            raise self.error("too many arguments to invert", source)

        self.pseudop_position_of(source)

        self.add_expression(expr)
        self.pseudop_unary_invert()


    @operator(_symbol_iter, iter)
    def operator_iter(self, source):
        try:
            called_by, (expr, rest) = source
        except ValueError:
            raise self.error("too few arguments to iter", source)

        if not is_nil(rest):
            raise self.error("too many arguments to iter", source)

        self.pseudop_position_of(source)

        self.add_expression(expr)
        self.pseudop_iter()


    def _helper_binary(self, source, opfun, flip=False):
        name, rest = source

        try:
            left, (right, rest) = rest

        except ValueError:
            raise self.error("too few arguments to %s" % name, source)

        if not is_nil(rest):
            raise self.error("too many arguments to %s" % name, source)

        self.pseudop_position_of(source)

        if flip:
            self.add_expression(right)
            self.add_expression(left)

        else:
            self.add_expression(left)
            self.add_expression(right)

        opfun()


    @operator(_symbol_lt, pyop.lt, _symbol_lt_)
    def operator_lt(self, source):
        """
        (< VAL1 VAL2)
        True if VAL1 is less-than VAL2
        """
        self._helper_binary(source, self.pseudop_compare_lt)


    @operator(_symbol_lte, pyop.le, _symbol_lte_)
    def operator_lte(self, source):
        """
        (<= VAL1 VAL2)
        True if VAL1 is less-than, or equal-to VAL2
        """

        self._helper_binary(source, self.pseudop_compare_lte)


    @operator(_symbol_eq, pyop.eq, _symbol_eq_)
    def operator_eq(self, source):
        """
        (== VAL1 VAL2)
        True if VAL1 and VAL2 are equal
        """

        self._helper_binary(source, self.pseudop_compare_eq)


    @operator(_symbol_not_eq, pyop.ne, _symbol_not_eq_)
    def operator_not_eq(self, source):
        """
        (!= VAL1 VAL2)
        True if VAL1 and VAL2 are not equal
        """

        self._helper_binary(source, self.pseudop_compare_not_eq)


    @operator(_symbol_gt, pyop.gt, _symbol_gt_)
    def operator_gt(self, source):
        """
        (>= VAL1 VAL2)
        True if VAL1 is greater-than VAL2
        """

        self._helper_binary(source, self.pseudop_compare_gt)


    @operator(_symbol_gte, pyop.ge, _symbol_gte_)
    def operator_gte(self, source):
        """
        (>= VAL1 VAL2)
        True if VAL1 is greater-than, or equal-to VAL2
        """

        self._helper_binary(source, self.pseudop_compare_gte)


    @operator(_symbol_in, pyop.contains)
    def operator_in(self, source):
        """
        (in SEQ VALUE)
        True if SEQ contains VALUE
        """

        self._helper_binary(source, self.pseudop_compare_in, True)


    @operator(_symbol_not_in, (lambda seq, value: value not in seq))
    def operator_not_in(self, source):
        """
        (not-in SEQ VALUE)
        False if SEQ contains VALUE
        """

        self._helper_binary(source, self.pseudop_compare_not_in, True)


    @operator(_symbol_is, pyop.is_)
    def operator_is(self, source):
        """
        (is OBJ1 OBJ2)
        True if OBJ1 and OBJ2 are the same object
        """

        self._helper_binary(source, self.pseudop_compare_is)


    @operator(_symbol_is_not, pyop.is_not)
    def operator_is_not(self, source):
        """
        (is-not OBJ1 OBJ2)
        True if OBJ1 and OBJ2 are different objects
        """

        self._helper_binary(source, self.pseudop_compare_is_not)


    @special(_symbol_attr)
    def special_get_attr(self, source):
        try:
            called_by, (obj, (member, rest)) = source
        except ValueError:
            raise self.error("too few arguments to attr", source)

        if not is_nil(rest):
            raise self.error("too many arguments to attr", source)

        self.pseudop_position_of(source)
        self.add_expression(obj)
        self.pseudop_getattr(str(member))

        # no further transformations
        return None


    @special(_symbol_set_attr)
    def special_set_attr(self, source):
        try:
            called_by, (obj, (member, (value, rest))) = source
        except ValueError:
            raise self.error("too few arguments to set-attr", source)

        if not is_nil(rest):
            raise self.error("too many arguments to set-attr", source)

        self.add_expression(obj)
        self.add_expression(value)
        self.pseudop_rot_two()
        self.pseudop_setattr(str(member))

        # make setf calls evaluate to None
        self.pseudop_const(None)

        # no further transformations
        return None


    @special(_symbol_quote)
    def special_quote(self, source):
        """
        Special form for quote
        """

        called_by, body = source

        if not body:
            self.error("Too fuew arguments to quote %s" % source, source)

        body, _rest = body

        if _rest:
            self.error("Too many arguments to quote %s" % source, source)

        self.pseudop_position_of(source)
        self.helper_quote(body)

        # no additional transform needed
        return None


    def helper_quote(self, body):
        if body is nil:
            self.pseudop_get_var("nil")

        elif is_keyword(body):
            self.helper_keyword(body)

        elif is_symbol(body):
            self.helper_symbol(body)

        elif is_pair(body):
            if is_proper(body):
                self.pseudop_get_var("make-proper")
            else:
                self.pseudop_get_var("cons")
            for cl, c in enumerate(body.unpack(), 1):
                self.helper_quote(c)
            self.pseudop_call(cl)

        else:
            self.pseudop_const(body)


    @special(_symbol_unquote)
    def special_unquote(self, source):
        raise self.error("unquote outside of quasiquote", source)


    @special(_symbol_splice)
    def special_splice(self, source):
        raise self.error("splice outside of quasiquote", source)


    @special(_symbol_quasiquote)
    def special_quasiquote(self, source):
        """
        Special form for quasiquote
        """

        called_by, (body, rest) = source

        if rest:
            raise self.error("Too many arguments to quasiquote", source)

        self.pseudop_position_of(source)
        self.helper_quasiquote(body)

        return None


    def helper_quasiquote(self, marked, level=0):
        # print("helper_quasiquote level:", level)
        # print("marked:", marked)

        if marked is nil or marked is _symbol_nil:
            self.pseudop_get_var("nil")
            return

        elif is_keyword(marked):
            self.helper_keyword(marked)
            return

        elif is_symbol(marked):
            self.helper_symbol(marked)
            return

        elif is_pair(marked):
            if is_proper(marked):
                head, tail = marked

                if head is _symbol_unquote:
                    tail, _rest = tail
                    if level == 0:
                        return self.add_expression(tail)
                    else:
                        self.pseudop_get_var("make-proper")
                        self.helper_symbol(head)
                        self.helper_quasiquote(tail, level - 1)
                        self.pseudop_call(2)
                        return

                elif head is _symbol_splice:
                    tail, _rest = tail
                    if level == 0:
                        self.pseudop_get_var("make-proper")
                        self.pseudop_get_var("to-tuple")
                        self.add_expression(tail)
                        self.pseudop_call(1)
                        self.pseudop_call_varargs(0)
                        return
                    else:
                        self.pseudop_get_var("make-proper")
                        self.helper_symbol(head)
                        self.helper_quasiquote(tail, level - 1)
                        self.pseudop_call(2)
                        return

                elif head is _symbol_quasiquote:
                    tail, _rest = tail
                    self.pseudop_get_var("make-proper")
                    self.helper_symbol(head)
                    self.helper_quasiquote(tail, level + 1)
                    self.pseudop_call(2)
                    return

                self.pseudop_get_var("make-proper")
            else:
                self.pseudop_get_var("cons")

            coll_tup = 0  # the count of collected tuples
            curr_tup = 0  # the size of the current tuple

            for expr in marked.unpack():
                if expr is nil or expr is _symbol_nil:
                    curr_tup += 1
                    self.pseudop_get_var("nil")
                    continue

                elif is_keyword(expr):
                    self.helper_keyword(expr)
                    curr_tup += 1
                    continue

                elif is_symbol(expr):
                    self.helper_symbol(expr)
                    curr_tup += 1
                    continue

                elif is_pair(expr):
                    if is_proper(expr):
                        head, tail = expr

                        if head is _symbol_quasiquote:
                            tail, _rest = tail
                            self.pseudop_get_var("make-proper")
                            self.helper_symbol(head)
                            self.helper_quasiquote(tail, level + 1)
                            self.pseudop_call(2)
                            curr_tup += 1
                            continue

                        elif head is _symbol_unquote:
                            u_expr, tail = tail

                            # print("unquote level:", level)
                            # print("expr:", u_expr)

                            if level == 0:
                                # either not proper or not splice
                                self.add_expression(u_expr)
                                curr_tup += 1
                                continue

                            else:
                                # not level 0, recurse with one less level
                                self.pseudop_get_var("make-proper")
                                self.helper_symbol(head)
                                self.helper_quasiquote(u_expr, level - 1)
                                self.pseudop_call(2)
                                curr_tup += 1
                                continue

                        elif head is _symbol_splice:
                            u_expr, tail = tail

                            if level == 0:
                                if curr_tup:
                                    self.pseudop_build_tuple(curr_tup)
                                    curr_tup = 0
                                    coll_tup += 1

                                self.pseudop_get_var("to-tuple")
                                self.add_expression(u_expr)
                                self.pseudop_call(1)
                                coll_tup += 1
                                continue

                            else:
                                self.pseudop_get_var("make-proper")
                                self.helper_symbol(head)
                                self.helper_quasiquote(u_expr, level - 1)
                                self.pseudop_call(2)
                                curr_tup += 1
                                continue

                    # a pair, but not an unquote
                    self.helper_quasiquote(expr, level)
                    curr_tup += 1
                    continue

                else:
                    # not a nil, symbol, or pair, so evaluates to its
                    # own self as a constant
                    self.pseudop_const(expr)
                    curr_tup += 1
                    continue

            # after iterating through the expressions of marked.unpack
            # we can check if we've accumulated anything.
            if curr_tup:
                self.pseudop_build_tuple(curr_tup)
                curr_tup = 0
                coll_tup += 1

            assert coll_tup, "no members accumulated"
            self.pseudop_build_tuple_unpack(coll_tup)
            self.pseudop_call_varargs(0)

        else:
            # some... other thing.
            self.pseudop_const(marked)


    @special(_symbol_begin)
    def special_begin(self, source):
        """
        Special form for begin
        """

        called_by, body = source

        self.helper_begin(body)

        # no additional transform needed
        return None


    def helper_begin(self, body):
        self.pseudop_position_of(body)

        if not body:
            # because all things are expressions, an empty begin still
            # needs to have a return value.
            self.pseudop_const(None)

        else:
            # a non-empty body needs to evaluate all of its child
            # expressions, but only keep the last one on the stack.
            while True:
                expr, body = body
                self.add_expression(expr)
                if body is nil:
                    break
                else:
                    self.pseudop_pop()

        return None


    @special(_symbol_with)
    def special_with(self, source):
        """
        Special form for managed context via with
        """

        called_by, (args, body) = source

        if args.count() != 2:
            msg = "with context must be binding and expression," \
                  " not %r" % args
            raise self.error(msg, cdr(source))

        binding, (expr, _rest) = args

        if not is_symbol(binding):
            msg = "binding must be a symbol, not %r" % binding
            raise self.error(msg, args)

        binding = str(binding)
        self.declare_var(str(binding))

        storage = self.gen_sym()
        self.declare_var(storage)

        label_cleanup = self.gen_label()

        self.add_expression(expr)
        self.pseudop_setup_with(label_cleanup)
        self.pseudop_faux_push(4)
        self.pseudop_set_var(binding)

        self.helper_begin(body)
        self.pseudop_set_var(storage)

        self.pseudop_pop_block()
        self.pseudop_const(None)
        self.pseudop_faux_pop()

        self.pseudop_label(label_cleanup)
        self.pseudop_with_cleanup_start()
        self.pseudop_with_cleanup_finish()
        self.pseudop_end_finally()

        self.pseudop_get_var(storage)
        self.pseudop_del_var(storage)
        self.pseudop_faux_pop(3)

        return None


    @special(_symbol_lambda)
    def special_lambda(self, source):
        """
        Special form for lambda
        """

        called_by, (args, body) = source

        self.pseudop_position_of(source)

        self.helper_function("<lambda>", args, body,
                             declared_at=self.position_of(source))

        # no additional transform needed
        return None


    @special(_symbol_function)
    def special_function(self, source):

        called_by, (namesym, cl) = source
        args, body = cl

        # todo create the function inside of a closure that has a
        # single local cell, which is the new function's name. this
        # will give the function the ability to reference its cell via
        # that cell.

        name = str(namesym)
        declared_at = self.position_of(source)

        self.pseudop_position_of(source)

        kid = self.child_context(declared_at=declared_at)
        with kid as subc:
            subc.declare_var(name)
            subc.helper_function(name, args, body, declared_at=declared_at)
            subc.pseudop_dup()
            subc.pseudop_set_var(name)
            subc.pseudop_return()
            code = subc.complete()

        self.pseudop_lambda(code)
        self.pseudop_call(0)

        # no additional transform needed
        return None


    @special(_symbol_let)
    def special_let(self, source):

        called_by, (bindings, body) = source

        args = []
        vals = []
        for arg in bindings.unpack():
            name, val = arg.unpack()
            args.append(str(name))
            vals.append(val)

        self.pseudop_position_of(source)

        self.helper_function("<let>", args, body,
                             declared_at=self.position_of(source))

        for val in vals:
            self.add_expression(val)

        self.pseudop_call(len(vals))

        # no additional transform needed
        return None


    def helper_function(self, name, args, body, declared_at=None):
        if is_symbol(args):
            varargs = True
            args = [str(args)]

        elif is_pair(args):
            varargs = not is_proper(args)
            args = map(str, args.unpack())

        elif isinstance(args, (list, tuple)):
            varargs = False
            args = map(str, args)

        else:
            msg = "formals must be symbol or pair, not %r" % type(args)
            raise self.error(msg, cl)

        if declared_at is None:
            declared_at = self.position_of(body)

        kid = self.child_context(args=args, varargs=varargs,
                                 name=name,
                                 declared_at=declared_at)

        with kid as subc:
            subc.helper_begin(body)
            subc.pseudop_return()
            code = subc.complete()

        self.pseudop_lambda(code)


    @special(_symbol_while)
    def special_while(self, source):
        called_by, (test, body) = source

        top = self.gen_label()
        done = self.gen_label()

        # pre-populate our return value
        self.pseudop_const(None)

        self.pseudop_label(top)

        self.add_expression(test)
        self.pseudop_pop_jump_if_false(done)

        # throw away previous value in favor of evaluating the body
        self.pseudop_pop()
        self.helper_begin(body)

        self.pseudop_jump(top)
        self.pseudop_label(done)

        # no additional transform needed
        return None


    @special(_symbol_raise)
    def special_raise(self, source):

        called_by, cl = source

        c = cl.count()
        if c > 3:
            msg = "too many arguments to raise %r" % cl
            raise self.error(msg, source)

        for rx in cl.unpack():
            self.add_expression(rx)

        self.pseudop_position_of(source)
        self.pseudop_raise(c)

        return None


    @special(_symbol_try)
    def special_try(self, source):

        kid = self.child_context(name="<try>",
                                 declared_at=self.position_of(source))

        with kid as subc:
            subc._helper_special_try(source)
            code = subc.complete()

        self.pseudop_lambda(code)
        self.pseudop_call(0)

        return None


    def _helper_special_try(self, source):

        called_by, (expr, catches) = source

        the_end = self.gen_label()

        has_finally = False
        has_else = False

        normal_catches = []

        self.pseudop_debug("top of _helper_special_try")

        # first, filter our catches down into normal exception
        # matches, finally, and else
        for ca in catches.unpack():
            ex, act = ca

            if not act:
                raise self.error("clause with no body in try", ca)

            if ex is _symbol_finally:
                if has_finally:
                    raise self.error("duplicate finally clause in try", ca)

                has_finally = True
                act_finally = act
                label_finally = self.gen_label()

            elif ex is _symbol_else:
                if has_else:
                    raise self.error("duplicate else clause in try", ca)
                has_else = True
                act_else = act
                label_else = self.gen_label()

            else:
                # this is a normal catch, where ex is a thing to
                # match, and act is the body to execute if it matches.
                normal_catches.append(ca)

        label_next = self.gen_label()

        if has_finally:
            # setup that finally block if we need one
            self.pseudop_setup_finally(label_finally)

        # here's our actual try block
        self.pseudop_setup_except(label_next)
        self.add_expression(expr)
        self.pseudop_debug("after try expression")

        if has_else:
            # the result of the expression is thrown away if we have
            # an else clause. This should be redundant, due to the
            # following pop_block, but let's be tidy
            self.pseudop_pop()
            self.pseudop_pop_block()
            self.pseudop_jump_forward(label_else)

        else:
            # but if there isn't an else clause, then the result of
            # the expression is the real deal. If there was a finally,
            # that will be jumped to.
            self.pseudop_return()
            self.pseudop_pop_block()
            self.pseudop_jump_forward(the_end)

        self.pseudop_debug("before handlers")

        # TOS exception type, TOS-1 exception, TOS-2 backtrace
        for ca in normal_catches:
            ex, act = ca

            # each attempt to match the exception should have us at
            # the TOS,TOS-1,TOS-2 setup
            # PLUS the 3 from an exception block
            self.pseudop_faux_push(7)

            self.pseudop_position_of(ca)
            self.pseudop_label(label_next)

            label_next = self.gen_label()

            self.pseudop_debug("beginning of declared handler")

            if is_pair(ex):
                # The exception is intended to be bound to a local
                # variable. To achieve that, we're going to set up
                # a lambda with that single binding, and stuff out
                # handler code inside of it.

                if not is_proper(ex):
                    raise self.error("non-proper biding in try", ex)

                match, (key, rest) = ex
                if rest:
                    # leftover arguments
                    raise self.error("too many bindings", ex)

                key = str(key)
                self.declare_var(key)

                cleanup = self.gen_label()

                # check if we match. If not, jump to the next catch
                # attempt
                self.pseudop_dup()
                self.add_expression(match)
                self.pseudop_compare_exception()
                self.pseudop_pop_jump_if_false(label_next)

                # okay, we've matched, so we need to bind the
                # exception instance to the key and clear the
                # other exception members off of the stack
                self.pseudop_pop()         # pops the dup'd exc type
                self.pseudop_set_var(key)  # binds the exc instance
                self.pseudop_pop()         # pops the traceback

                # wrap our handler code in a finally block, so
                # that we can delete the key from the local
                # namespace afterwards
                self.pseudop_setup_finally(cleanup)

                # handle the exception, return the result
                self.helper_begin(act)
                self.pseudop_return()
                self.pseudop_faux_pop(4)  # trigger the finally

                # the return triggers the finally block to jump to
                # here. This ensures a value in the key and then
                # deletes it. Necessary just in case the handler
                # code also deleted the key (don't want to try
                # deleting it twice)
                self.pseudop_label(cleanup)
                self.pseudop_faux_push()  # the finally pushes a return
                self.pseudop_const(None)
                self.pseudop_set_var(key)
                self.pseudop_del_var(key)

                # end_finally pops our cleanup block and
                # finishes returning
                self.pseudop_end_finally()
                self.pseudop_return_none()

            else:
                # this is an exception match without a binding
                self.pseudop_dup()
                self.add_expression(ex)
                self.pseudop_compare_exception()
                self.pseudop_pop_jump_if_false(label_next)

                # Now let's throw all that stuff away.
                self.pseudop_pop()
                self.pseudop_pop()
                self.pseudop_pop()

                self.helper_begin(act)
                self.pseudop_return()

            # yay we handled it!
            self.pseudop_pop_except()
            self.pseudop_debug("handled declared exception")
            self.pseudop_jump_forward(the_end)

        # after all the attempts at trying to match the exception, we
        # land here. This is the catch-all fallback, that will
        # re-raise the exception.
        self.pseudop_label(label_next)
        self.pseudop_faux_push(4)
        self.pseudop_debug("restored stack in fall-through")
        self.pseudop_faux_pop(3)
        self.pseudop_end_finally()
        self.pseudop_debug("fall-through exception")

        if has_else:
            # okay, we've arrived at the else handler. Let's run it,
            # and return that value.
            self.pseudop_label(label_else)
            self.pseudop_debug("start of else handler")
            self.helper_begin(act_else)

            # if there is a finally registered, this will trigger it
            # to run (and possibly overwrite the return value)
            self.pseudop_return()
            self.pseudop_debug("end of else handler")

        if has_finally:
            # here's the actual handling of the finally event. This
            # will get jumped to from the returns above, if the
            # finally block was registered.
            self.pseudop_label(the_end)
            self.pseudop_pop_block()

            self.pseudop_const(None)
            self.pseudop_faux_pop()

            # run the handler and overwrite the return value with its
            # result
            self.pseudop_label(label_finally)
            self.pseudop_faux_push(1)  # implicit from the finally block
            self.helper_begin(act_finally)
            self.pseudop_return()

            # and close off the finally block
            self.pseudop_debug("closing off finally")
            self.pseudop_end_finally()

        else:
            self.pseudop_label(the_end)
            self.pseudop_return_none()

        self.pseudop_debug("at the end")

        # no further transformations needed on the passed source code.
        return None


    @special(_symbol_setq)
    def special_setq(self, source):

        called_by, (binding, body) = source

        if not is_symbol(binding):
            raise self.error("assignment must be by symbolic name",
                             cdr(source))

        value, rest = body
        if not is_nil(rest):
            raise self.error("extra values in assignment", source)

        if not is_pair(value):
            self.pseudop_position_of(body)

        self.add_expression(value)
        self.pseudop_set_var(str(binding))

        # set-var calls should evaluate to None
        self.pseudop_const(None)

        # no additional transform needed
        return None


    @special(_symbol_global)
    def special_global(self, source):

        called_by, (binding, rest) = source
        if not is_nil(rest):
            raise self.error("extra values in global lookup", source)

        self.pseudop_position_of(source)
        self.pseudop_get_global(str(binding))

        return None


    @special(_symbol_define_global, _symbol_define)
    def special_define_global(self, source):

        called_by, (binding, body) = source

        self.helper_begin(body)

        assert is_symbol(binding), "define-global with non-symbol binding"

        self.pseudop_position_of(source)
        self.pseudop_define_global(str(binding))

        # define expression evaluates to None
        self.pseudop_const(None)

        return None


    @special(_symbol_define_local)
    def special_define_local(self, source):

        called_by, (binding, body) = source

        self.helper_begin(body)

        assert is_symbol(binding), "define-local with non-symbol binding"

        self.pseudop_position_of(source)
        self.pseudop_define_local(str(binding))

        # define expression evaluates to None
        self.pseudop_const(None)

        return None


    # @special(_symbol_defmacro)
    # def special_defmacro(self, source):

    #     called_by, (namesym, cl) = source
    #     name = str(namesym)

    #     args, body = cl

    #     if is_symbol(args):
    #         args = [str(args)]
    #         varargs = True

    #     elif is_pair(args):
    #         varargs = not is_proper(args)
    #         args = map(str, args.unpack())

    #     else:
    #         msg = "formals must be symbol or pair, not %r" % type(args)
    #         raise self.error(msg, cl)

    #     kid = self.child_context(args=args, varargs=varargs,
    #                              name=name,
    #                              declared_at=self.position_of(source))

    #     with kid as subc:
    #         subc.helper_begin(body)
    #         subc.pseudop_return()
    #         code = subc.complete()

    #     self.pseudop_get_var("macro")
    #     self.pseudop_lambda(code)
    #     self.pseudop_call(1)

    #     self.pseudop_define_global(name)

    #     # defmacro expression evaluates to None
    #     self.pseudop_const(None)

    #     # no additional transform needed
    #     return None


    @special(_symbol_cond)
    def special_cond(self, source):

        called_by, cl = source

        done = self.gen_label()
        label = self.gen_label()

        for test, body in cl.unpack():
            self.pseudop_label(label)
            label = self.gen_label()

            if test is _symbol_else:
                # print(repr(body))
                self.helper_begin(body)
                self.pseudop_jump_forward(done)
                break

            else:
                self.add_expression(test)
                self.pseudop_pop_jump_if_false(label)
                self.helper_begin(body)
                self.pseudop_jump_forward(done)

        else:
            # there was no else statement, so add a catch-all
            self.pseudop_label(label)
            self.pseudop_const(None)

        self.pseudop_label(done)

        return None


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
            # what we found doesn't qualify, throw it away
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


def lnt_compile(lnt, firstline=None):
    # print("lnt_compile")
    # print( "lnt:", lnt)
    # print( "firstline:", firstline)

    if not lnt:
        return (1 if firstline is None else firstline), b''

    firstline = lnt[0][1] if firstline is None else firstline
    gathered = []

    # print( "firstline:", firstline)

    prev_offset = 0
    prev_line = firstline

    for offset, line, _col in lnt:
        if gathered and line == prev_line:
            continue

        d_offset = (offset - prev_offset)
        d_line = (line - prev_line)

        d_offset &= 0xff

        if d_line < 0:
            if (3, 6) <= version_info:
                # in version 3.6 and beyond, negative line numbers
                # work fine, so a CALL_FUNCTION can correctly state
                # that it happens at line it started on, rather than
                # on the line it closes at
                pass
            else:
                # before version 3.6, negative relative line numbers
                # weren't possible. Thus the line of a CALL_FUNCTION
                # is the line it closes on, rather than the line it
                # begins on. So we'll skip this lnt entry.
                continue

        if d_line < -128 or d_line > 127:
            dd_line = (d_line >> 8) & 0xff
            gathered.append(bytes([d_offset, dd_line]))

        d_line &= 0xff
        gathered.append(bytes([d_offset, d_line]))

        prev_offset = offset
        prev_line = line

    res = firstline, b''.join(gathered)
    # print("result:", res)
    return res


def label_generator(formatstr="label_%04i"):
    counter = 0

    def gen_label():
        nonlocal counter
        counter += 1
        return formatstr % counter

    return gen_label


@contextmanager
def temporary_specials(env, **kwds):
    specs = dict((key, TemporarySpecial(value, name=key))
                 for key, value in kwds.items())

    unset = object()
    try:
        old = dict((key, env.get(key, unset)) for key in specs.keys())
        env.update(specs)
        yield

    except Exception:
        raise

    finally:
        for value in specs.values():
            value.expire()

        for key, value in old.items():
            if value is unset:
                del env[key]
            else:
                env[key] = value


class TemporarySpecial(Special):
    def expire(self):
        self.special = self.__dead__

    def __dead__(self):
        raise Exception("temporary special invoked outside of its limits")


def builtin_specials():
    for sym, meth in SpecialCodeSpace.all_specials():
        yield str(sym), meth


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
