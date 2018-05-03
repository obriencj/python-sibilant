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
sibilant.compiler

The sibilant compiler. Converts expressions into Python bytecode.

author: Christopher O'Brien <obriencj@gmail.com>
license: LGPL v.3
"""


import dis
import threading

from abc import ABCMeta, abstractmethod
from contextlib import contextmanager
from enum import Enum
from functools import partial, partialmethod
from itertools import count
from os.path import exists
from platform import python_implementation
from sys import version_info
from types import CodeType
from typing import Union

from ..lib import (
    SibilantException, SibilantSyntaxError,
    symbol, is_symbol,
    lazygensym,
    keyword, is_keyword,
    cons, nil, is_pair, is_proper,
    get_position, fill_position,
)


__all__ = (
    "UnsupportedVersion",
    "CompilerSyntaxError",
    "Opcode", "Pseudop", "Block",
    "CodeSpace", "ExpressionCodeSpace",
    "code_space_for_version",
    "Compiled", "is_compiled",
    "Special", "is_special",
    "Macro", "is_macro",
    "Alias", "is_alias",
    "Operator", "is_operator",
    "gather_formals", "gather_parameters",
)


Symbol = Union[lazygensym, symbol]


_keyword_star = keyword("*")
_keyword_starstar = keyword("**")


COMPILER_DEBUG = False

# this is an amount to pad out all max_stack allocations
STACK_SAFETY = 2

_active = threading.local()


def current():
    try:
        return _active.compiler
    except AttributeError:
        _active.compiler = None
        return None


def set_current(compiler):
    _active.compiler = compiler


class CompilerException(Exception):
    pass


class CompilerSyntaxError(SibilantSyntaxError):
    pass


class UnsupportedVersion(SibilantException):
    pass


class Compiled():
    __slots__ = ("__name__", )
    __objname__ = "sibilant compiled"


    def __init__(self, name):
        self.__name__ = name


    def __call__(self, *args, **kwds):
        msg = "Attempt to call %r as a runtime function" % self
        raise TypeError(msg)


    def __repr__(self):
        return "<%s %r>" % (self.__objname__, self.__name__)


    def compile(self, compiler, source_obj):
        pass


def is_compiled(obj):
    return isinstance(obj, Compiled)


class Special(Compiled):
    __objname__ = "special form"


    def __init__(self, name, compilefn):
        super().__init__(name)


    def __new__(cls, name, compilefn):
        if not callable(compilefn):
            msg = "compilefn must be callable, not %r" % compilefn
            raise SibilantException(msg)

        nom = str(name or compilefn.__name__)
        mbs = {
            "__doc__": compilefn.__doc__,
            "compile": staticmethod(compilefn),
        }
        cls = type(nom, (cls, ), mbs)
        return object.__new__(cls)


def is_special(obj):
    return isinstance(obj, Special)


class Macro(Compiled):
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
        self._proper = True


    def __new__(cls, name, expandfn):
        if not callable(expandfn):
            msg = "expandfn must be callable, not %r" % expandfn
            raise SibilantException(msg)

        nom = str(name or expandfn.__name__)
        mbs = {
            "__doc__": expandfn.__doc__,
            "expand": staticmethod(expandfn),
        }
        cls = type(nom, (cls, ), mbs)
        return object.__new__(cls)


    def compile(self, compiler, source_obj, tc=False):
        called_by, source = source_obj

        if self._proper:
            position = source_obj.get_position()
            args, kwargs = simple_parameters(source, position)
            expr = self.expand(*args, **kwargs)

        else:
            data = list(source.unpack())
            expr = self.expand(*data)

        fill_position(expr, source_obj.get_position())

        # a Macro should always evaluate to some kind of non-None. If
        # all the work of the macro was performed in the environment
        # for some reason, it's still expected to provide an expanded
        # result, or stack underruns are almost guaranteed. Therefore
        # if the wrapped macro function returns None we will pretend
        # it expanded to the None symbol instead.
        return _symbol_None if expr is None else expr


def is_macro(obj):
    return isinstance(obj, Macro)


class Alias(Macro):
    __objname__ = "alias"


    def compile(self, compiler, source_obj, tc=False):
        expanded = self.expand()
        expanded = _symbol_None if expanded is None else expanded

        if is_symbol(source_obj):
            return expanded

        elif is_pair(source_obj):
            called_by, source = source_obj
            res = cons(expanded, source)
            fill_position(res, source_obj.get_position())
            return res

        else:
            msg = "Error expanding alias %s from %r" % \
                  (self.__name__, source_obj)
            compiler.error(msg, source_obj)


def is_alias(obj):
    return isinstance(obj, Alias)


class Operator(Compiled):
    __objname__ = "operator"


    def __init__(self, name, compilefn, runtimefn):
        super().__init__(name)


    def __new__(cls, name, compilefn, runtimefn):
        assert(compilefn is not None)
        assert(runtimefn is not None)

        if not callable(compilefn):
            msg = "compilefn must be callable, not %r" % compilefn
            raise SibilantException(msg)

        if not callable(runtimefn):
            msg = "runtimefn must be callable, not %r" % runtimefn
            raise SibilantException(msg)

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


class OpcodeEnum(Enum):

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

    def stack_effect(self, arg=None):
        return dis.stack_effect(self.value, arg)


Opcode = OpcodeEnum("Opcode", dis.opmap)


# Python 3.6 has this in the enum module, but I support 3.5 so I'll
# just make my own.
_auto = partial(next, count())


class Pseudop(Enum):
    BINARY_ADD = _auto()
    BINARY_AND = _auto()
    BINARY_FLOOR_DIVIDE = _auto()
    BINARY_LSHIFT = _auto()
    BINARY_MATRIX_MULTIPLY = _auto()
    BINARY_MODULO = _auto()
    BINARY_MULTIPLY = _auto()
    BINARY_OR = _auto()
    BINARY_POWER = _auto()
    BINARY_RSHIFT = _auto()
    BINARY_SUBTRACT = _auto()
    BINARY_TRUE_DIVIDE = _auto()
    BINARY_XOR = _auto()
    BLOCK = _auto()
    BREAK_LOOP = _auto()
    BUILD_LIST = _auto()
    BUILD_MAP = _auto()
    BUILD_MAP_UNPACK = _auto()
    BUILD_SET = _auto()
    BUILD_SLICE = _auto()
    BUILD_STR = _auto()
    BUILD_TUPLE = _auto()
    BUILD_TUPLE_UNPACK = _auto()
    CALL = _auto()
    CALL_KW = _auto()
    CALL_VAR = _auto()
    CALL_VAR_KW = _auto()
    COMPARE_OP = _auto()
    CONST = _auto()
    CONTINUE_LOOP = _auto()
    DEBUG_STACK = _auto()
    DEL_ATTR = _auto()
    DEL_GLOBAL = _auto()
    DEL_ITEM = _auto()
    DEL_VAR = _auto()
    DUP = _auto()
    END_FINALLY = _auto()
    FAUX_PUSH = _auto()
    FOR_ITER = _auto()
    FORMAT = _auto()
    GET_ATTR = _auto()
    GET_GLOBAL = _auto()
    GET_ITEM = _auto()
    GET_VAR = _auto()
    GET_YIELD_FROM_ITER = _auto()
    IMPORT_NAME = _auto()
    IMPORT_FROM = _auto()
    ITER = _auto()
    JUMP = _auto()
    JUMP_FORWARD = _auto()
    LABEL = _auto()
    LAMBDA = _auto()
    POP = _auto()
    POP_BLOCK = _auto()
    POP_EXCEPT = _auto()
    POP_JUMP_IF_FALSE = _auto()
    POP_JUMP_IF_TRUE = _auto()
    POSITION = _auto()
    RAISE = _auto()
    RET_VAL = _auto()
    ROT_THREE = _auto()
    ROT_TWO = _auto()
    SETUP_EXCEPT = _auto()
    SETUP_FINALLY = _auto()
    SETUP_LOOP = _auto()
    SETUP_WITH = _auto()
    SET_ATTR = _auto()
    SET_GLOBAL = _auto()
    SET_ITEM = _auto()
    SET_LOCAL = _auto()
    SET_VAR = _auto()
    UNARY_INVERT = _auto()
    UNARY_NEGATIVE = _auto()
    UNARY_NOT = _auto()
    UNARY_POSITIVE = _auto()
    UNPACK_EX = _auto()
    UNPACK_SEQUENCE = _auto()
    WITH_CLEANUP_FINISH = _auto()
    WITH_CLEANUP_START = _auto()
    YIELD_FROM = _auto()
    YIELD_VAL = _auto()


_auto = partial(next, count())


class Block(Enum):
    BASE = _auto()
    BEGIN = _auto()
    EXCEPT = _auto()
    EXCEPT_MATCH = _auto()
    FINALLY = _auto()
    FINALLY_CLEANUP = _auto()
    LOOP = _auto()
    TRY = _auto()
    WITH = _auto()
    WITH_CLEANUP = _auto()


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


# a bunch of commonly used symbols, so we don't have to try and
# recreate over and over.

_symbol_nil = symbol("nil")
_symbol_None = symbol("None")
_symbol_True = symbol("True")
_symbol_False = symbol("False")
_symbol_ellipsis = symbol("...")

_symbol_attr = symbol("attr")


def _label_generator(formatstr="label_%04i"):
    counter = 0

    def gen_label():
        nonlocal counter
        counter += 1
        return formatstr % counter

    return gen_label


class CodeBlock():
    def __init__(self, block_type, init_stack=0, leftovers=0):
        self.pseudops = []
        self.children = []
        self.block_type = block_type
        self.init_stack = init_stack
        self.allow_leftovers = leftovers
        self.top_label = None
        self.pop_label = None
        self.storage = None


    def child(self, block_type, init_stack=0, leftovers=0):
        child_block = type(self)(block_type, init_stack, leftovers)
        self.children.append(child_block)
        self.pseudops.append((Pseudop.BLOCK, child_block))
        return child_block


    def clear(self):
        self.pseudops.clear()
        for block in self.children:
            block.clear()
        self.children.clear()
        self.top_label = None
        self.pop_label = None
        self.storage = None


    def __del__(self):
        del self.pseudops
        del self.children


    def gen_pseudops(self):
        pb = Pseudop.BLOCK

        for op, *args in self.pseudops:
            if op is pb:
                yield from args[0].gen_pseudops()
            else:
                yield (op, *args)


    def max_stack(self, code):
        """
        Calculates the maximum stack size from the pseudo operations. This
        function is total crap, but it's good enough for now.
        """

        leftovers = self.allow_leftovers

        maxc = self.init_stack
        stac = self.init_stack
        labels = {}

        index = 0

        _Pseudop = Pseudop

        def push(by=1):
            nonlocal maxc, stac
            stac += by
            if stac > maxc:
                maxc = stac

        def pop(by=1):
            nonlocal stac
            nonlocal index
            stac -= by
            if stac < 0:
                print("SHIT BROKE in ", self.block_type)
                for i, o in enumerate(self.pseudops, 1):
                    if i == index:
                        print(" -->\t", repr(o))
                    else:
                        print("\t", repr(o))

            assert (stac >= 0), "max_stack counter underrun at %i" % index

        # print("enter max_stack()", self.block_type)
        for opa in self.pseudops:
            op, *args = opa

            index += 1
            # print(op, args, stac, maxc)

            if op is _Pseudop.POSITION:
                continue

            elif op is _Pseudop.DEBUG_STACK:
                print(" ".join(map(str, args)),
                      "max:", maxc, "current:", stac)

            # elif op is _Pseudop.MAGIC_POP_ALL:
                # print("MAGIC_POP_ALL:", stac)

                # this is a stupid hack when I need to unwind a block
                # like for continue.
                # opa[1] = stac

            elif op is _Pseudop.LABEL:
                stac = labels.get(args[0], stac)
                # pass

            elif op is _Pseudop.BLOCK:
                block = args[0]
                block_i, block_max = block.max_stack(code)
                push(block_max)
                push(block_i)
                pop(block_max)

            # These ops have to be here so they can see the stac
            # counter value
            elif op in (_Pseudop.JUMP,
                        _Pseudop.JUMP_FORWARD):
                labels[args[0]] = stac
                pass

            elif op in (_Pseudop.POP_JUMP_IF_TRUE,
                        _Pseudop.POP_JUMP_IF_FALSE):
                pop()
                labels[args[0]] = stac

            else:
                # defer everything else so it can be overridden
                # depending on Python version
                code.helper_max_stack(op, args, push, pop)

        if stac != leftovers:
            print("leaving max_stack()", self.block_type, "with", stac)
            for o in self.pseudops:
                print("\t", repr(o))

        assert (stac == leftovers), ("%i / %i left-over stack items for %r"
                                     % (stac, leftovers, self.block_type))

        return stac, maxc


class Mode(Enum):
    EXPRESSION = "expr"
    MODULE = "module"


class CodeSpace(metaclass=ABCMeta):
    """
    Represents a lexical scope, expressions occurring within that
    scope, and nested sub-scopes.
    """

    def __init__(self, parent=None, name=None, args=(), kwonly=0,
                 varargs=False, varkeywords=False,
                 filename=None, declared_at=None,
                 tco_enabled=True, mode=Mode.EXPRESSION):

        self.env = None
        self.parent = parent
        self.name = name

        self.mode = mode

        self.tco_enabled = tco_enabled
        self.tailcalls = 0

        self.generator = False

        self.filename = filename
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
            # n = str(arg)
            _list_unique_append(self.args, arg)
            _list_unique_append(self.fast_vars, arg)

        self.kwonly = kwonly
        self.varargs = varargs
        self.varkeywords = varkeywords

        # this holds a combination of global var keys and member
        # attribute keys
        self.names = []

        # first const is required -- it'll be None or a doc string and
        # then None
        self.consts = [None]

        self.blocks = [CodeBlock(Block.BASE, 0, 0)]

        if parent:
            self.gen_label = parent.gen_label

        else:
            self.gen_label = _label_generator()


    def __del__(self):
        del self.env
        del self.parent
        del self.blocks
        del self.gen_label
        self.consts.clear()
        del self.consts


    def reset(self):
        self.env = None

        if self.blocks:
            self.blocks[0].clear()
        self.blocks = [CodeBlock(Block.BASE, 0, 0)]
        self.consts = [None]

        self.tailcalls = 0
        self.generator = False

        self.fast_vars.clear()
        self.free_vars.clear()
        self.cell_vars.clear()
        self.global_vars.clear()
        self.names.clear()


    def gen_pseudops(self):
        assert self.blocks, "empty block stack"
        base = self.blocks[0]
        assert base.block_type is Block.BASE, "incorrect base block type"
        yield from base.gen_pseudops()


    def _push_block(self, block_type, init_stack=0, leftovers=0):
        self.require_active()
        assert self.blocks, "no code blocks in stack"

        old = self.blocks[-1]
        block = old.child(block_type, init_stack, leftovers)
        self.blocks.append(block)

        block.top_label = self.gen_label()
        block.pop_label = self.gen_label()
        block.storage = None

        return block


    def _pop_block(self):
        return self.blocks.pop()


    @contextmanager
    def block_loop(self, break_label=None):
        if break_label is None:
            # no specified break label, set one up
            _break_label = self.gen_label()
        else:
            _break_label = break_label

        block = self._push_block(Block.LOOP, 0, 0)
        self.pseudop_setup_loop(_break_label)
        self.pseudop_debug(" == enter loop ==")
        self.pseudop_label(block.top_label)
        yield block
        self.pseudop_label(block.pop_label)
        self.pseudop_debug(" == exit loop ==")
        self.pseudop_pop_block()

        if break_label is None:
            # no specified break label, so we need to
            # ensure the label we created exists past the pop
            self.pseudop_label(_break_label)

        self._pop_block()


    @contextmanager
    def block_finally(self, cleanup_label):
        block = self._push_block(Block.FINALLY, 0, 0)
        self.pseudop_setup_finally(cleanup_label)
        self.pseudop_debug(" == enter finally ==")
        self.pseudop_label(block.top_label)
        yield block
        self.pseudop_label(block.pop_label)
        self.pseudop_debug(" == exit finally ==")
        self.pseudop_pop_block()
        self.pseudop_faux_pop(6)
        self._pop_block()


    @contextmanager
    def block_finally_cleanup(self, cleanup_label):
        block = self._push_block(Block.FINALLY_CLEANUP, 0, 0)
        self.pseudop_const(None)
        self.pseudop_label(cleanup_label)
        self.pseudop_debug(" == enter finally_cleanup ==")
        self.pseudop_label(block.top_label)
        yield block
        self.pseudop_label(block.pop_label)
        self.pseudop_debug(" == exit finally_cleanup ==")
        self.pseudop_end_finally()
        self._pop_block()


    @contextmanager
    def block_try(self, except_label):
        block = self._push_block(Block.TRY, 0, 0)
        self.pseudop_setup_except(except_label)
        self.pseudop_debug(" == enter try ==")
        self.pseudop_label(block.top_label)
        yield block
        self.pseudop_label(block.pop_label)
        self.pseudop_debug(" == exit try ==")
        self.pseudop_pop_block()
        self.pseudop_faux_pop(6)
        self._pop_block()


    @contextmanager
    def block_except(self, except_label):
        block = self._push_block(Block.EXCEPT, 1, 0)
        self.pseudop_label(except_label)
        self.pseudop_debug(" == enter except ==")
        self.pseudop_label(block.top_label)
        yield block
        self.pseudop_label(block.pop_label)
        self.pseudop_debug(" == exit except ==")
        self.pseudop_end_finally()
        self._pop_block()


    @contextmanager
    def block_except_match(self, except_label):
        block = self._push_block(Block.EXCEPT_MATCH, 7, 0)
        self.pseudop_label(except_label)
        self.pseudop_debug(" == enter except_match ==")
        self.pseudop_label(block.top_label)
        yield block
        self.pseudop_label(block.pop_label)
        self.pseudop_debug(" == exit except_match ==")
        self.pseudop_pop_except()
        self.pseudop_faux_pop(4)
        self._pop_block()


    @contextmanager
    def block_begin(self):
        block = self._push_block(Block.BEGIN, 0, 1)
        self.pseudop_debug(" == enter begin ==")
        self.pseudop_label(block.top_label)
        yield block
        self.pseudop_label(block.pop_label)
        self.pseudop_debug(" == exit begin ==")
        self._pop_block()


    @contextmanager
    def block_with(self, expr):
        cleanup_label = self.gen_label()
        block = self._push_block(Block.WITH, 0, 0)
        self.add_expression(expr)
        self.pseudop_setup_with(cleanup_label)
        self.pseudop_debug(" == enter with ==")
        self.pseudop_label(block.top_label)
        yield block
        self.pseudop_label(block.pop_label)
        self.pseudop_debug(" == exit with ==")
        self.pseudop_debug(" == enter with_cleanup ==")
        self.pseudop_pop_block()
        self.pseudop_const(None)
        self.pseudop_label(cleanup_label)
        self.pseudop_with_cleanup_start()
        self.pseudop_with_cleanup_finish()
        self.pseudop_end_finally()
        self.pseudop_debug(" == exit with_cleanup ==")
        self.pseudop_faux_pop(7)
        self._pop_block()


    def get_block(self):
        return self.blocks[-1]


    def get_block_top_label(self):
        return self.get_block().top_label


    def get_block_pop_label(self):
        return self.get_block().top_label


    def get_block_storage(self):
        return self.get_block().storage


    def set_block_storage(self, value):
        self.get_block().storage = value


    def declare_generator(self):
        self.generator = True

        if self.tailcalls:
            msg = "generator mode set after a tailcall was declared"
            raise self.error(msg)


    def declare_tailcall(self):
        assert self.tco_enabled, "declare_tailcall without tco_enabled"
        assert not self.generator, "declare_tailcall with a generator"
        self.tailcalls += 1


    def _gensym_predicate(self, sym: symbol):
        # sym = str(sym)
        return (sym not in self.args and
                sym not in self.fast_vars and
                sym not in self.free_vars and
                sym not in self.cell_vars and
                sym not in self.global_vars)


    def gensym(self, name=None):
        """
        produces a deferral which will turn into a gensym call
        """

        return lazygensym(name, self._gensym_predicate)


    def set_doc(self, docstr: str):
        """
        Python expects doc strings to be the first constant (followed
        immediately by None) in a code object's const pool.
        """

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
            raise CompilerException("compiler code space is not active")


    @contextmanager
    def activate(self, env):
        """
        Binds to an environment, clears self at end of context
        """

        self.env = env
        old_compiler = current()
        set_current(self)

        try:
            yield self

        finally:
            set_current(old_compiler)
            self.reset()


    def child(self, name=None, declared_at=None, **addtl):

        """
        Returns a child codespace
        """

        if declared_at is None:
            declared_at = self.declared_at

        if name is None:
            name = "%s.<child>" % self.name

        addtl["name"] = name
        addtl["declared_at"] = declared_at

        addtl.setdefault("filename", self.filename)
        addtl.setdefault("tco_enabled", self.tco_enabled)

        return type(self)(parent=self, **addtl)


    def child_context(self, **kwargs):
        """
        Returns an active context for a child codespace
        """

        self.require_active()
        cs = self.child(**kwargs)
        return cs.activate(self.env)


    def declare_const(self, value):
        """
        Declare a constant value
        """

        assert (type(value) in _CONST_TYPES), "invalid const type %r" % value
        _list_unique_append(self.consts, value)


    def declare_var(self, namesym: Symbol):
        """
        Declare a local variable by name
        """

        # name = str(namesym)
        # assert is_symbol(namesym)

        if self.mode is Mode.MODULE:
            return self.request_global(namesym)

        else:
            if not (namesym in self.cell_vars or namesym in self.free_vars):
                _list_unique_append(self.fast_vars, namesym)


    def request_var(self, namesym: Symbol):
        """
        State that this code space wants to consume a var by name.

        If the var is already declared locally, do nothing.
        Otherwise, ff the var is in our ancestry, it will request
        those parents convert it into a cell so that it will be
        available as a closure here.  If the var is neither local nor
        in our ancestry, then we'll presume it must be a global
        reference.
        """

        # assert is_symbol(namesym)

        # name = str(name)
        if (namesym in self.fast_vars) or \
           (namesym in self.free_vars) or \
           (namesym in self.cell_vars) or \
           (namesym in self.global_vars):

            # either the name is already available in this scope as a
            # load_fast, or we've already figured out whether it needs
            # to be found via a load_closure or load_global
            pass

        else:
            # we need to figure out if access to this var will be via
            # a load_closure, or load_global call

            if self.parent and self.parent.request_cell(namesym):
                # we asked our parent if we can get it as a closure,
                # and they said yes
                _list_unique_append(self.free_vars, namesym)
            else:
                self.request_global(namesym)


    def request_global(self, namesym: Symbol):
        # assert is_symbol(namesym)

        _list_unique_append(self.global_vars, namesym)
        _list_unique_append(self.names, namesym)


    def request_cell(self, namesym: Symbol):
        # assert is_symbol(namesym)

        if namesym in self.global_vars:
            # no, we won't provide a cell for a global
            return False

        elif ((namesym in self.cell_vars) or
              (namesym in self.free_vars)):
            # yup, we can provide that cell. It's either already a
            # cell we created to give away, or a cell we ourselves
            # already inherited.
            return True

        elif namesym in self.fast_vars:
            # we need to convert this fast var into a cell var for our
            # child namespace to use
            # self.fast_vars.remove(name)
            _list_unique_append(self.cell_vars, namesym)
            return True

        elif self.parent and self.parent.request_cell(namesym):
            # we asked our parent and they had it, so now it's a cell
            # for them, and a free for us, and we can affirm that we
            # can provide it
            _list_unique_append(self.free_vars, namesym)
            return True

        else:
            # nope, there's no local in the nested namespace
            # inheritance to convert into a cell
            return False


    def request_name(self, namesym: Symbol):
        # assert is_symbol(namesym)

        _list_unique_append(self.names, namesym)


    def helper_prep_varargs(self):
        # initial step which will convert the pythonic varargs tuple
        # into a proper cons list

        assert self.varargs, "helper_prep_varargs called when varargs=False"

        if self.varkeywords:
            # if it's varargs and also varkeywords, the var will be
            # the second-to-last one.
            offset = -2

        else:
            # if it's a varargs, but not varkeywords, the var will be
            # the last one.
            offset = -1

        varname = self.args[offset]
        # assert (is_symbol(varname))

        if self.declared_at:
            self.pseudop_position(*self.declared_at)

        self.pseudop_get_var(symbol("build-proper"))
        self.pseudop_get_var(varname)
        self.pseudop_call_var(0, 0)
        self.pseudop_set_var(varname)


    def pseudop(self, *op_and_args):
        """
        Pushes a pseudo op and arguments into the code
        """

        assert self.blocks, "no blocks on stack"
        self.blocks[-1].pseudops.append(op_and_args)


    def _op(argname, arg=None, *, _pseudop=pseudop):
        # this is a temportary helper to shrink the declarations of
        # the simpler operators. It's deleted after the operators are
        # all defined
        if arg is None:
            return partialmethod(_pseudop, Pseudop[argname])
        else:
            return partialmethod(_pseudop, Pseudop[argname], arg)


    if COMPILER_DEBUG:
        def pseudop_debug(self, *op_args):
            self.pseudop(Pseudop.DEBUG_STACK, *op_args)
    else:
        def pseudop_debug(self, *op_args):
            pass


    def pseudop_get_attr(self, namesym: Symbol):
        # assert is_symbol(namesym)
        self.request_name(namesym)
        self.pseudop(Pseudop.GET_ATTR, namesym)


    def pseudop_set_attr(self, namesym: Symbol):
        # assert is_symbol(namesym)
        self.request_name(namesym)
        self.pseudop(Pseudop.SET_ATTR, namesym)


    def pseudop_del_attr(self, namesym: Symbol):
        # assert is_symbol(namesym)
        self.request_name(namesym)
        self.pseudop(Pseudop.DEL_ATTR, namesym)


    def pseudop_faux_push(self, count=1):
        self.pseudop(Pseudop.FAUX_PUSH, count)


    def pseudop_faux_pop(self, count=1):
        self.pseudop(Pseudop.FAUX_PUSH, -count)


    def pseudop_position(self, line, column):
        self.pseudop(Pseudop.POSITION, line, column)


    def pseudop_position_of(self, cl):
        position = get_position(cl, None)
        if position:
            assert (type(position) is tuple), "non-tuple position"
            self.pseudop(Pseudop.POSITION, *position)


    def pseudop_call(self, argc, kwdc=0):
        self.pseudop(Pseudop.CALL, argc, kwdc)


    def pseudop_call_var(self, argc, kwdc=0):
        self.pseudop(Pseudop.CALL_VAR, argc, kwdc)


    def pseudop_call_kw(self, argc, kwdc=0):
        self.pseudop(Pseudop.CALL_KW, argc, kwdc)


    def pseudop_call_var_kw(self, argc, kwdc=0):
        self.pseudop(Pseudop.CALL_VAR_KW, argc, kwdc)


    def pseudop_const(self, val):
        """
        Pushes a pseudo op to load a constant value
        """
        self.declare_const(val)
        self.pseudop(Pseudop.CONST, val)


    def pseudop_set_local(self, namesym: Symbol):
        """
        Declares var as local, assigns TOS to is
        """
        # assert is_symbol(namesym)
        self.declare_var(namesym)
        self.pseudop(Pseudop.SET_LOCAL, namesym)


    def pseudop_get_var(self, namesym: Symbol):
        """
        Pushes a pseudo op to load a named value
        """
        # assert is_symbol(namesym)
        self.request_var(namesym)
        self.pseudop(Pseudop.GET_VAR, namesym)


    def pseudop_set_var(self, namesym: Symbol):
        """
        Pushes a pseudo op to assign to a named value
        """
        # assert is_symbol(namesym)
        self.request_var(namesym)
        self.pseudop(Pseudop.SET_VAR, namesym)


    def pseudop_del_var(self, namesym: Symbol):
        # assert is_symbol(namesym)
        self.request_var(namesym)
        self.pseudop(Pseudop.DEL_VAR, namesym)


    def pseudop_lambda(self, code, *params):
        """
        Pushes a pseudo op to load a lambda from code
        """

        self.declare_const(code)
        self.declare_const(code.co_name)
        self.pseudop(Pseudop.LAMBDA, code, *params)


    def pseudop_pop(self, count=1):
        assert count > 0, ("pseudop_pop with weird count %r" % count)

        while count > 0:
            self.pseudop(Pseudop.POP)
            count -= 1


    def pseudop_unpack_sequence(self, argc):
        self.pseudop(Pseudop.UNPACK_SEQUENCE, argc)


    def pseudop_unpack_ex(self, left, right):
        self.pseudop(Pseudop.UNPACK_EX, left, right)


    def pseudop_return_none(self):
        """
        Pushes a pseudo op to return None
        """
        self.pseudop_const(None)
        self.pseudop(Pseudop.RET_VAL)


    def pseudop_yield(self):
        self.declare_generator()
        self.pseudop(Pseudop.YIELD_VAL)


    def pseudop_yield_none(self):
        self.declare_generator()
        self.pseudop_const(None)
        self.pseudop(Pseudop.YIELD_VAL)


    def pseudop_yield_from(self):
        self.declare_generator()
        self.pseudop(Pseudop.YIELD_FROM)


    def pseudop_get_global(self, namesym: Symbol):
        # assert is_symbol(namesym)
        self.request_global(namesym)
        self.pseudop(Pseudop.GET_GLOBAL, namesym)


    def pseudop_set_global(self, namesym: Symbol):
        """
        Pushes a pseudo op to globally define TOS to name
        """
        # assert is_symbol(namesym)
        self.request_global(namesym)
        self.pseudop(Pseudop.SET_GLOBAL, namesym)


    def pseudop_del_global(self, namesym: Symbol):
        # assert is_symbol(namesym)
        self.request_global(namesym)
        self.pseudop(Pseudop.DEL_GLOBAL, namesym)


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


    def pseudop_setup_loop(self, done_label):
        self.pseudop(Pseudop.SETUP_LOOP, done_label)


    def pseudop_setup_with(self, try_label):
        self.pseudop(Pseudop.SETUP_WITH, try_label)


    def pseudop_setup_except(self, try_label):
        self.pseudop(Pseudop.SETUP_EXCEPT, try_label)


    def pseudop_setup_finally(self, final_label):
        self.pseudop(Pseudop.SETUP_FINALLY, final_label)


    def pseudop_raise(self, count=0):
        self.pseudop(Pseudop.RAISE, count)


    def pseudop_build_str(self, count):
        self.pseudop(Pseudop.BUILD_STR, count)


    def pseudop_format(self, flags):
        self.pseudop(Pseudop.FORMAT, flags)


    def pseudop_build_slice(self, count):
        # assert (1 < count < 4)
        self.pseudop(Pseudop.BUILD_SLICE, count)


    def pseudop_build_tuple(self, count):
        self.pseudop(Pseudop.BUILD_TUPLE, count)


    def pseudop_build_list(self, count):
        self.pseudop(Pseudop.BUILD_LIST, count)


    def pseudop_build_set(self, count):
        self.pseudop(Pseudop.BUILD_SET, count)


    def pseudop_build_tuple_unpack(self, count):
        self.pseudop(Pseudop.BUILD_TUPLE_UNPACK, count)


    def pseudop_build_map(self, count):
        self.pseudop(Pseudop.BUILD_MAP, count)


    def pseudop_build_map_unpack(self, count):
        self.pseudop(Pseudop.BUILD_MAP_UNPACK, count)


    def pseudop_import_name(self, namesym: Symbol):
        # assert is_symbol(namesym)
        self.request_name(namesym)
        self.pseudop(Pseudop.IMPORT_NAME, namesym)


    def pseudop_import_from(self, namesym: Symbol):
        # assert is_symbol(namesym)
        self.request_name(namesym)
        self.pseudop(Pseudop.IMPORT_FROM, namesym)


    pseudop_with_cleanup_start = _op("WITH_CLEANUP_START")
    pseudop_with_cleanup_finish = _op("WITH_CLEANUP_FINISH")
    pseudop_break_loop = _op("BREAK_LOOP")
    pseudop_continue_loop = _op("CONTINUE_LOOP")

    pseudop_pop_block = _op("POP_BLOCK")
    pseudop_pop_except = _op("POP_EXCEPT")
    pseudop_end_finally = _op("END_FINALLY")

    pseudop_iter = _op("ITER")
    pseudop_for_iter = _op("FOR_ITER")
    pseudop_get_yield_from_iter = _op("GET_YIELD_FROM_ITER")

    pseudop_rot_two = _op("ROT_TWO")
    pseudop_rot_three = _op("ROT_THREE")
    pseudop_dup = _op("DUP")
    pseudop_return = _op("RET_VAL")

    pseudop_get_item = _op("GET_ITEM")
    pseudop_set_item = _op("SET_ITEM")
    pseudop_del_item = _op("DEL_ITEM")

    pseudop_unary_positive = _op("UNARY_POSITIVE")
    pseudop_unary_negative = _op("UNARY_NEGATIVE")
    pseudop_unary_not = _op("UNARY_NOT")
    pseudop_unary_invert = _op("UNARY_INVERT")

    pseudop_binary_add = _op("BINARY_ADD")
    pseudop_binary_subtract = _op("BINARY_SUBTRACT")
    pseudop_binary_multiply = _op("BINARY_MULTIPLY")
    pseudop_binary_matrix_multiply = _op("BINARY_MATRIX_MULTIPLY")
    pseudop_binary_divide = _op("BINARY_TRUE_DIVIDE")
    pseudop_binary_floor_divide = _op("BINARY_FLOOR_DIVIDE")
    pseudop_binary_power = _op("BINARY_POWER")
    pseudop_binary_modulo = _op("BINARY_MODULO")
    pseudop_binary_lshift = _op("BINARY_LSHIFT")
    pseudop_binary_rshift = _op("BINARY_RSHIFT")
    pseudop_binary_and = _op("BINARY_AND")
    pseudop_binary_or = _op("BINARY_OR")
    pseudop_binary_xor = _op("BINARY_XOR")

    pseudop_compare_lt = _op("COMPARE_OP", 0)
    pseudop_compare_lte = _op("COMPARE_OP", 1)
    pseudop_compare_eq = _op("COMPARE_OP", 2)
    pseudop_compare_not_eq = _op("COMPARE_OP", 3)
    pseudop_compare_gt = _op("COMPARE_OP", 4)
    pseudop_compare_gte = _op("COMPARE_OP", 5)
    pseudop_compare_in = _op("COMPARE_OP", 6)
    pseudop_compare_not_in = _op("COMPARE_OP", 7)
    pseudop_compare_is = _op("COMPARE_OP", 8)
    pseudop_compare_is_not = _op("COMPARE_OP", 9)
    pseudop_compare_exception = _op("COMPARE_OP", 10)

    del _op


    def helper_max_stack(self, op, args, push, pop):

        _Pseudop = Pseudop
        _Opcode = Opcode

        if op in (_Pseudop.CONST,
                  _Pseudop.DUP,
                  _Pseudop.GET_VAR,
                  _Pseudop.GET_GLOBAL,
                  _Pseudop.BREAK_LOOP,
                  _Pseudop.FOR_ITER,
                  _Pseudop.IMPORT_FROM,
                  _Pseudop.CONTINUE_LOOP):
            push()

        elif op in (_Pseudop.DEL_VAR,
                    _Pseudop.DEL_GLOBAL):
            pass

        elif op in (_Pseudop.GET_ATTR,
                    _Pseudop.UNARY_POSITIVE,
                    _Pseudop.UNARY_NEGATIVE,
                    _Pseudop.UNARY_NOT,
                    _Pseudop.UNARY_INVERT,
                    _Pseudop.ITER,
                    _Pseudop.GET_YIELD_FROM_ITER,
                    _Pseudop.YIELD_VAL):
            pop()
            push()

        elif op in (_Pseudop.SET_ATTR,
                    _Pseudop.DEL_ITEM):
            pop(2)

        elif op is _Pseudop.SET_ITEM:
            pop(3)

        elif op in (_Pseudop.SET_GLOBAL,
                    _Pseudop.SET_LOCAL,
                    _Pseudop.SET_VAR,
                    _Pseudop.RET_VAL,
                    _Pseudop.YIELD_FROM,
                    _Pseudop.DEL_ATTR,
                    _Pseudop.POP):
            pop()

        elif op is _Pseudop.LAMBDA:
            pop(args[1])
            a = len(args[0].co_freevars)
            if a:
                push(a)
                pop(a)
            push(2)
            pop(2)
            push()

        elif op is _Pseudop.BUILD_MAP:
            pop(args[0] * 2)
            push()

        elif op is _Pseudop.UNPACK_SEQUENCE:
            pop()
            push(args[0])

        elif op is _Pseudop.UNPACK_EX:
            pop()
            push(args[0] + args[1] + 1)

        elif op in (_Pseudop.BUILD_LIST,
                    _Pseudop.BUILD_SET,
                    _Pseudop.BUILD_STR,
                    _Pseudop.BUILD_TUPLE,
                    _Pseudop.BUILD_TUPLE_UNPACK,
                    _Pseudop.BUILD_MAP_UNPACK):
            pop(args[0])
            push()

        elif op is _Pseudop.SETUP_EXCEPT:
            push(_Opcode.SETUP_EXCEPT.stack_effect(1))

        elif op is _Pseudop.SETUP_WITH:
            push(_Opcode.SETUP_WITH.stack_effect(1))

        elif op is _Pseudop.SETUP_FINALLY:
            push(_Opcode.SETUP_FINALLY.stack_effect(1))

        elif op is _Pseudop.SETUP_LOOP:
            push(_Opcode.SETUP_LOOP.stack_effect(1))

        elif op is _Pseudop.POP_BLOCK:
            push(_Opcode.POP_BLOCK.stack_effect())

        elif op is _Pseudop.POP_EXCEPT:
            push(_Opcode.POP_EXCEPT.stack_effect())

        elif op is _Pseudop.WITH_CLEANUP_START:
            push(_Opcode.WITH_CLEANUP_START.stack_effect())

        elif op is _Pseudop.WITH_CLEANUP_FINISH:
            push(_Opcode.WITH_CLEANUP_FINISH.stack_effect())

        elif op is _Pseudop.END_FINALLY:
            push(_Opcode.END_FINALLY.stack_effect())

        elif op in (_Pseudop.COMPARE_OP,
                    _Pseudop.GET_ITEM,
                    _Pseudop.BINARY_ADD,
                    _Pseudop.BINARY_SUBTRACT,
                    _Pseudop.BINARY_MULTIPLY,
                    _Pseudop.BINARY_MATRIX_MULTIPLY,
                    _Pseudop.BINARY_TRUE_DIVIDE,
                    _Pseudop.BINARY_FLOOR_DIVIDE,
                    _Pseudop.BINARY_POWER,
                    _Pseudop.BINARY_MODULO,
                    _Pseudop.BINARY_LSHIFT,
                    _Pseudop.BINARY_RSHIFT,
                    _Pseudop.BINARY_AND,
                    _Pseudop.BINARY_XOR,
                    _Pseudop.BINARY_OR,
                    _Pseudop.IMPORT_NAME):
            pop(2)
            push()

        elif op in (_Pseudop.RAISE,
                    _Pseudop.BUILD_SLICE):
            pop(args[0])
            push()   # we pretend RAISE evaluates to something

        elif op is _Pseudop.FAUX_PUSH:
            push(args[0])

        elif op in (_Pseudop.ROT_THREE,
                    _Pseudop.ROT_TWO):
            pass

        else:
            assert False, "unknown pseudop %r" % op


    def max_stack(self):
        leftovers, maximum = self.blocks[0].max_stack(self)
        assert leftovers == 0, "code has leftovers on stack"
        return maximum


    def complete(self):
        """
        Produces a python code object representing the state of this
        CodeSpace
        """

        # self.require_active()

        argcount = len(self.args)

        stacksize = self.max_stack() + max(1, STACK_SAFETY)

        flags = CodeFlag.OPTIMIZED.value | CodeFlag.NEWLOCALS.value

        if self.varargs:
            argcount -= 1
            flags |= CodeFlag.VARARGS.value

        if self.varkeywords:
            argcount -= 1
            flags |= CodeFlag.VARKEYWORDS.value

        kwonly = self.kwonly
        argcount -= kwonly

        if not (self.free_vars or self.cell_vars):
            flags |= CodeFlag.NOFREE.value

        if self.parent:
            flags |= CodeFlag.NESTED.value

        if self.generator:
            flags |= CodeFlag.GENERATOR.value

        lnt = []
        code = self.code_bytes(lnt)

        consts = tuple(self.consts)

        names = tuple(map(str, self.names))

        varnames = list(self.fast_vars)
        for v in self.cell_vars:
            _list_unique_append(varnames, v)
        varnames = tuple(map(str, varnames))

        nlocals = len(varnames)

        filename = "<sibilant>" if self.filename is None else self.filename

        name = "<anon>" if self.name is None else self.name

        firstlineno = self.declared_at[0] if self.declared_at else None
        firstlineno, lnotab = self.lnt_compile(lnt, firstline=firstlineno)

        freevars = tuple(map(str, self.free_vars))
        cellvars = tuple(map(str, self.cell_vars))

        ret = CodeType(argcount, kwonly, nlocals, stacksize, flags, code,
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
        # TODO : user some sort of introspection instead of having to
        # write an import for every case...

        if (3, 7) <= ver <= (3, 8):
            from .cpython37 import CPython37
            return CPython37

        elif (3, 6) <= ver <= (3, 7):
            from .cpython36 import CPython36
            return CPython36

        elif (3, 5) <= ver <= (3, 6):
            from .cpython35 import CPython35
            return CPython35

    return None


class ExpressionCodeSpace(CodeSpace):
    """
    Adds support for expressions, operators, macros, and special forms
    to the basic functionality of CodeSpace
    """


    def add_expression(self, expr, tc=False):
        """
        Insert an expression into the code space. expr should be a cons
        cell representing the expression. If the expression appears to
        be a special form (either a macro defined in the CodeSpace's
        env, or a pre-defined built-in special), it will be expanded
        and compiled to pseudo ops.
        """

        tc = tc and self.tco_enabled

        self.require_active()

        if expr is None:
            self.pseudop_const(None)
            return

        self.pseudop_position_of(expr)

        while expr is not None:
            if expr is nil:
                # convert nil expressions to a literal nil
                self.pseudop_get_var(symbol("nil"))
                expr = None

            elif is_pair(expr):
                try:
                    expr = self.compile_pair(expr, tc)
                except TypeError as te:
                    msg = "while compiling pair %r" % expr
                    raise self.error(msg, expr) from te

            elif is_symbol(expr):
                try:
                    expr = self.compile_symbol(expr, tc)
                except TypeError as te:
                    msg = "while compiling symbol %r" % expr
                    raise self.error(msg, expr) from te

            elif is_keyword(expr):
                expr = self.compile_keyword(expr)

            else:
                # TODO there are some literal types that can't be used
                # as constants, will need to fill those in here. For
                # now we're just presuming it's going to be a
                # constant, the end.
                expr = self.pseudop_const(expr)

            if is_compiled(expr):
                msg = "leftover higher-order macro %r" % expr
                raise CompilerException(msg)

        return None


    def add_expression_with_return(self, expr):
        """
        Insert an expression, then an op to return its result from the
        current call
        """
        self.add_expression(expr)
        self.pseudop_return()


    def compile_pair(self, expr, tc=False):
        self.require_active()

        if not is_proper(expr):
            print("compile_pair improper:", str(expr))
            raise self.error("cannot evaluate improper lists as expressions",
                             expr)

        self.pseudop_position_of(expr)
        position = expr.get_position()
        head, tail = expr

        if is_symbol(head):
            # see if this is a a compiled call
            comp = self.find_compiled(head)
            if comp:
                # yup. We'll just report that we've expanded to that
                return comp.compile(self, expr, tc)

            head = self.compile_symbol(head)
            if head is None:
                # fall out of this nonsense
                pass
            elif is_compiled(head):
                # head evaluated at compile-time to a higher-order macro
                namesym = symbol(head.__name__)
                return head.compile(self, cons(namesym, tail), tc)
            else:
                return cons(head, tail)

        elif is_proper(head):
            head = self.compile_pair(head)
            if head is None:
                # fall out of this nonsense
                pass
            elif is_compiled(head):
                # head evaluated at compile-time to a higher-order macro
                namesym = symbol(head.__name__)
                return head.compile(self, cons(namesym, tail), tc)
            else:
                return cons(head, tail)

        else:
            # head was neither a proper nor a symbol... wtf was it?
            # probably an error, so let's try and actually add it as
            # an expression and let it blow up.
            self.add_expression(head, tc)

        # if we made it this far, head has already been compiled and
        # returned None (meaning it was just a plain-ol expression),
        # so we can proceed with normal apply semantics
        self.compile_call_tos(tail, position, tc)

        return None


    def compile_call_tos(self, args, position=None, tc=False):
        if tc:
            self.helper_tailcall_tos(args, position)
        self.helper_compile_call(args, position)


    def helper_tailcall_tos(self, args, position):
        """
        Should be invoked upon TOS functions objects that could
        potentially recur. tc indicates whether the TOS is in a valid
        tailcall position.
        """

        if not self.tco_enabled or \
           self.generator or \
           self.mode is Mode.MODULE:
            return False

        self.helper_tailrecur_tos(args, position)

        # either this isn't a self-referential function, or it is and
        # the tailcall isn't recursive, so we'll use the trampoline
        # instead.
        self.pseudop_get_var(symbol("tailcall"))
        self.pseudop_rot_two()
        self.pseudop_call(1)

        self.declare_tailcall()


    def helper_tailrecur_tos(self, args, position):
        # test for possible recursion optimization.

        # first check, make sure we've got a self-ref available. If we
        # don't then we cannot ensure it's actually a recursive call
        # at runtime, so a jump0 is unsafe.
        if not (self.free_vars and self.free_vars[0] is symbol("")):
            return

        # next, let's see if the parameters being passed line up with
        # the formals we're set up with

        parameters = gather_parameters(args, position)
        pos, kwds, vals, star, starstar = parameters

        if self.varargs or self.varkeywords or star or starstar:
            # no support for variadics, it's too tricky to inline. A
            # trampoline bounce isn't any slower than us calling to
            # other functions to figure out how to reform variadics
            # prior to a jump0, so just let the trampoline do its
            # bounce.
            return

        self_args = self.args

        # first, skim off the argument name bindings for the
        # positional arguments
        bindings = self_args[:len(pos)]

        # now we need to go through the keyword arguments in order and
        # record their binding
        for arg in map(str, kwds):
            if arg in bindings:
                # keyword parameter dups positional parameter name,
                # fall back on trampoline
                return
            elif arg not in self_args:
                # unknown keyword parameter, fall back on trampoline
                return
            else:
                bindings.append(arg)

        if len(self_args) != len(bindings):
            # if the bindings and argument count doesn't line up, then
            # again it's easier to just let the trampoline bounce and
            # have python figure out the args (and potentially raise a
            # TypeError). Default values take too much effort for now.
            return

        # if we made it this far, then we have a mapping of the
        # arguments we were given to the argument names, and thus can
        # evaluate in order and assign to the correct local variable,
        # then jump to 0, provided a quick runtime sanity check
        # passes, verifying that we are indeed calling the same
        # function that we're already executing.

        tclabel = self.gen_label()

        self.pseudop_dup()
        self.pseudop_get_var(symbol(""))
        self.pseudop_compare_is()
        self.pseudop_pop_jump_if_false(tclabel)
        self.pseudop_pop()

        # evaluate all of the arguments in order
        for arg in pos:
            self.add_expression(arg, False)
        for arg in vals:
            self.add_expression(arg, False)

        # now bind them to the vars that we discovered
        for var in reversed(bindings):
            self.pseudop_set_var(var)

        self.pseudop_jump(0)
        self.pseudop_label(tclabel)


    @abstractmethod
    def helper_compile_call(self, tail, position):
        """
        The function to be called is presumed to already be on the stack
        before this is helper is invoked. The helper should assemble the
        arguments as necessary on the stack, and then push the pseudops
        to evaluate them and finally to call the function.
        """

        pass


    def compile_symbol(self, sym: symbol, tc=False):
        """
        The various ways that a symbol on its own can evaluate.
        """

        # assert (is_symbol(sym))

        self.require_active()

        comp = self.find_compiled(sym)
        if comp and is_alias(comp):
            return comp.compile(self, sym, tc)

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
                return self.pseudop_get_var(sym)
            else:
                return cons(_symbol_attr, *ex, nil)


    def compile_keyword(self, kwd):
        # it should be fairly rare that a keyword is actually
        # passed anywhere at runtime -- it's mostly meant for use
        # as a marker in source expressions for specials.

        self.pseudop_get_var(symbol("keyword"))
        self.pseudop_const(str(kwd))
        self.pseudop_call(1)
        return None


    def error(self, message, source):

        text = None

        pos = None
        if is_pair(source) and source is not nil:
            pos = source.get_position()

        if pos and exists(self.filename):
            with open(self.filename, "rt") as fin:
                for text, _lineno in zip(fin, range(0, pos[0])):
                    # print(" ...", text)
                    pass

        if not text:
            text = str(source)
            pos = (pos[0], 0)

        return CompilerSyntaxError(message, pos, text=text,
                                   filename=self.filename)


    def find_compiled(self, namesym: Symbol):
        return _find_compiled(self.env, namesym)


def _list_unique_append(onto_list, value: symbol):
    # we have to manually loop and use the `is` operator, because the
    # list.index method will match False with 0 and True with 1, which
    # incorrectly collapses consts pools when both values are present

    for index, found in enumerate(onto_list):
        if found is value:
            return index
    else:
        onto_list.append(value)
        return len(onto_list) - 1


def compile_expression(source_obj, env, filename="<anon>",
                       **codespace_args):

    """
    Compile and yield a Python code object representing the evaluation
    of the given source_obj expression, which should be the result of
    a a `read` call from a SourceStream, a valid symbol or cons pair,
    or a self-evaluating type.
    """

    factory = code_space_for_version()
    if not factory:
        raise UnsupportedVersion(version_info)

    codespace = factory(filename=filename, **codespace_args)
    with codespace.activate(env):
        codespace.add_expression_with_return(source_obj)
        code = codespace.complete()

    return code


def gather_formals(args, declared_at=None, filename=None):
    """
    parses formals pair args into five values:
    (positional, keywords, defaults, stararg, starstararg)

    - positional is a list of symbols defining positional arguments

    - defaults is a list of keywords and expr pairs defining keyword
      arguments and their default value expression

    - kwonly is a list of keywords and expr pairs which are
      keyword-only arguments and theid default value expression

    - stararg is a symbol for variadic positional arguments

    - starstararg is a symbol for variadic keyword arguments
    """

    undefined = object()

    def err(msg):
        return SibilantSyntaxError(msg, location=declared_at,
                                   filename=filename)

    if is_symbol(args):
        return ((), (), (), args, None)

    elif isinstance(args, (list, tuple)):
        improper = False
        args = cons(*args, nil)

    elif is_proper(args):
        improper = False

    elif is_pair(args):
        improper = True

    else:
        raise err("formals must be symbol or pair, not %r" % args)

    positional = []

    iargs = iter(args.unpack())
    for arg in iargs:
        if is_keyword(arg):
            if improper:
                raise err("cannot mix improper formal with keywords")
            else:
                break
        elif is_symbol(arg):
            positional.append(arg)
        else:
            raise err("positional formals must be symbols, nor %r" % arg)
    else:
        # handled all of args, done deal.
        if improper:
            return (positional[:-1], (), (), positional[-1], None)
        else:
            return (positional, (), (), None, None)

    defaults = []
    kwonly = []

    while arg not in (_keyword_star, _keyword_starstar):
        value = next(iargs, undefined)
        if value is undefined:
            raise err("missing value for keyword formal %s" % args)
        else:
            defaults.append((arg, value))

        arg = next(iargs, undefined)

        if arg is undefined:
            break
        elif is_keyword(arg):
            continue
        else:
            raise err("keyword formals must be alternating keywords and"
                      " values, not %r" % arg)

    star = None
    starstar = None

    if arg is undefined:
        return (positional, defaults, kwonly, None, None)

    if arg is _keyword_star:
        star = next(iargs, undefined)
        if star is undefined:
            raise err("* keyword requires symbol binding")
        elif star is nil:
            # nil means an ignored star arg, this is allowed.
            pass
        elif not is_symbol(star):
            raise err("* keyword requires symbol binding, not %r" % star)
        arg = next(iargs, undefined)

    if arg is undefined:
        return (positional, defaults, kwonly, star, starstar)

    # while is_symbol(arg):
    #     kwonly.append(arg)
    #     arg = next(iargs, undefined)
    #
    # if arg is undefined:
    #     return (positional, defaults, kwonly, star, starstar)

    if not is_keyword(arg):
        raise err("expected keyword in formals, got %r" % arg)

    # keyword formals after *: are considered keyword-only
    while arg not in (_keyword_star, _keyword_starstar):
        value = next(iargs, undefined)
        if value is undefined:
            raise err("missing value for keyword-only formal %s" % arg)
        else:
            kwonly.append((arg, value))

        arg = next(iargs, undefined)
        if arg is undefined:
            break
        elif is_keyword(arg):
            continue
        else:
            raise err("keyword-only formals must be alternating keywords"
                      " and values, not %r" % arg)

    if arg is _keyword_starstar:
        starstar = next(iargs, undefined)
        if starstar is undefined:
            raise err("** keyword requires symbol binding")
        elif not is_symbol(starstar):
            raise err("** keyword requires symbol binding, not %r" % star)
        arg = next(iargs, undefined)

    if arg is not undefined:
        raise err("leftover formals %r" % arg)

    return (positional, defaults, kwonly, star, starstar)


def simple_parameters(source_args, declared_at=None):
    parameters = gather_parameters(source_args, declared_at)
    pos, kwds, vals, star, starstar = parameters

    args = list(pos)
    if star:
        args.extend(star)

    kwargs = dict(zip(map(str, kwds), vals))
    if starstar:
        kwargs.update(starstar)

    return args, kwargs


def gather_parameters(args, declared_at=None, filename=None):
    """
    parses parameter args into five values:
    (positional, keywords, values, stararg, starstararg)

    - positional is a list of expressions for positional arguments
    - keywords is a list of keywords defining keyword arguments
    - values is a list of expressions defining values for keywords
    - stararg is a symbol for variadic positional expression
    - starstararg is a symbol for variadic keyword expression
    """

    undefined = object()

    def err(msg):
        return SibilantSyntaxError(msg, location=declared_at,
                                   filename=filename)

    if is_symbol(args):
        return ((), (), (), args, None)

    elif isinstance(args, (list, tuple)):
        improper = False
        args = cons(*args, nil) if args else nil

    elif is_proper(args):
        improper = False

    elif is_pair(args):
        improper = True

    else:
        raise err("parameters must be symbol or pair, not %r" % args)

    positional = []

    iargs = iter(args.unpack())
    for arg in iargs:
        if is_keyword(arg):
            break
        else:
            positional.append(arg)
    else:
        # handled all of args, done deal.
        if improper:
            return (positional[:-1], (), (), positional[-1], None)
        else:
            return (positional, (), (), None, None)

    keywords = []
    defaults = []

    while arg not in (_keyword_star, _keyword_starstar):
        keywords.append(arg)

        value = next(iargs, undefined)
        if value is undefined:
            raise err("missing value for keyword parameter %s" % arg)
        else:
            defaults.append(value)

        arg = next(iargs, undefined)
        if arg is undefined:
            break
        elif is_keyword(arg):
            continue
        else:
            raise err("keyword parameters must be alternating keywords and"
                      " values, not %r" % arg)

    star = None
    starstar = None

    if arg is undefined:
        return (positional, keywords, defaults, None, None)

    if arg is _keyword_star:
        star = next(iargs, undefined)
        if star is undefined:
            raise err("* keyword parameter needs value")
        arg = next(iargs, undefined)

    if arg is _keyword_starstar:
        starstar = next(iargs, undefined)
        if starstar is undefined:
            raise err("** keyword parameter needs value")
        arg = next(iargs, undefined)

    if arg is not undefined:
        raise err("leftover parameters %r" % arg)

    return (positional, keywords, defaults, star, starstar)


def unpack_formals(args, kwds,
                   positionals, variadic, kwonly,
                   keywords, defaults, kwvariadic):

    lpos = len(positionals)
    largs = len(args)
    kwvar = dict(kwds)

    if lpos == largs:
        args = list(args)
        var = () if variadic else None
    elif lpos < largs:
        if variadic:
            args = list(args[:lpos])
            var = args[lpos:]
        else:
            raise TypeError("too many positional arguments")
    else:
        args = list(args)
        var = () if variadic else None
        try:
            for a in positionals[largs:]:
                args.append(kwvar.pop(a))
        except KeyError:
            raise TypeError("missing required argument %s" % a)

    kwds = {}
    try:
        for a in kwonly:
            kwds[a] = kwvar.pop(a)
    except KeyError:
        raise TypeError("missing required keyword-only argument %s" % a)

    for a in keywords:
        kwds[a] = kwvar.pop(a, defaults[a])

    if kwvar and not kwvariadic:
        raise TypeError("unexpected arguments, %r" % list(kwvar.keys()))

    return args, var, kwds, (kwvar if kwvariadic else None)


def _find_compiled(env, namesym: Symbol):
    # okay, let's look through the environment by name
    name = str(namesym)

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


def _get_expander(env, source_obj):
    expander = None

    if source_obj is nil:
        return None

    elif is_symbol(source_obj):
        namesym = source_obj
        found = _find_compiled(env, namesym)
        if is_alias(found):
            expander = found.expand

    elif is_proper(source_obj):
        namesym, params = source_obj

        if is_symbol(namesym):
            found = _find_compiled(env, namesym)
            if is_alias(found):
                def expander():
                    expanded = cons(found.expand(), params)
                    expanded.set_position(source_obj.get_position())
                    return expanded

            elif is_macro(found):
                if found._proper:
                    # FIXME: this is some garbage right here. we need
                    # to make sure macros aren't being invoked this
                    # way with a variadic.
                    position = params.get_position()
                    args, kwargs = simple_parameters(params, position)
                    expander = partial(found.expand, *args, **kwargs)
                else:
                    expander = partial(found.expand, *params.unpack())

    return expander


def iter_macroexpand(env, source_obj, position=None):
    if env is None:
        compiler = current()
        env = None if compiler is None else compiler.env

    if env is None:
        raise CompilerException("macroexpand requires non-None env when"
                                " no compiler is active")

    if position is None and is_pair(source_obj):
        position = source_obj.get_position()

    expander = _get_expander(env, source_obj)
    while expander:
        expanded = expander()
        yield expanded

        expander = _get_expander(env, expanded)
        if is_pair(expanded):
            fill_position(expanded, position)


def macroexpand(env, source_obj, position=None):
    for source_obj in iter_macroexpand(env, source_obj, position):
        pass
    return source_obj


def macroexpand_1(env, source_obj, position=None):
    return next(iter_macroexpand(env, source_obj, position), source_obj)


#
# The end.
