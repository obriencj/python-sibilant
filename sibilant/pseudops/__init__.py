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
sibilant.pseudops

The generalize bytecode operations for sibilant. These Pseudo Opcodes
translate to implementation- and version-specific opcodes for cpython.

author: Christopher O'Brien <obriencj@gmail.com>
license: LGPL v.3
"""


import dis

from abc import ABCMeta, abstractmethod
from contextlib import contextmanager
from enum import Enum
from functools import partial, partialmethod
from types import CodeType
from typing import Union


try:
    from enum import auto
except ImportError:
    # 3.5 doesn't have it, so let's make our own
    from itertools import count
    auto = partial(next, count())


from ..lib import SibilantException, symbol, lazygensym


__all__ = (
    "PseudopsCompiler",
    "Opcode", "Pseudop", "Block",
    "CONST_TYPES",
)


# what you put in the consts tuple of a code object.
CONST_TYPES = (
    CodeType,
    str, bytes,
    tuple, list, dict, set,
    bool, int, float, complex,
    type(None), type(...),
)


Symbol = Union[lazygensym, symbol]
Constant = Union[CONST_TYPES]


COMPILER_DEBUG = False

# this is an amount to pad out all max_stack allocations
STACK_SAFETY = 2


class PseudopsException(SibilantException):
    pass


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


class Block(Enum):
    BASE = auto()
    BEGIN = auto()
    EXCEPT = auto()
    EXCEPT_MATCH = auto()
    FINALLY = auto()
    FINALLY_CLEANUP = auto()
    LOOP = auto()
    TRY = auto()
    WITH = auto()
    WITH_CLEANUP = auto()


class Pseudop(Enum):
    BINARY_ADD = auto()
    BINARY_AND = auto()
    BINARY_FLOOR_DIVIDE = auto()
    BINARY_LSHIFT = auto()
    BINARY_MATRIX_MULTIPLY = auto()
    BINARY_MODULO = auto()
    BINARY_MULTIPLY = auto()
    BINARY_OR = auto()
    BINARY_POWER = auto()
    BINARY_RSHIFT = auto()
    BINARY_SUBTRACT = auto()
    BINARY_TRUE_DIVIDE = auto()
    BINARY_XOR = auto()
    BLOCK = auto()
    BREAK_LOOP = auto()
    BUILD_LIST = auto()
    BUILD_MAP = auto()
    BUILD_MAP_UNPACK = auto()
    BUILD_SET = auto()
    BUILD_SLICE = auto()
    BUILD_STR = auto()
    BUILD_TUPLE = auto()
    BUILD_TUPLE_UNPACK = auto()
    CALL = auto()
    CALL_KW = auto()
    CALL_VAR = auto()
    CALL_VAR_KW = auto()
    COMPARE_OP = auto()
    CONST = auto()
    CONTINUE_LOOP = auto()
    DEBUG_STACK = auto()
    DEL_ATTR = auto()
    DEL_GLOBAL = auto()
    DEL_ITEM = auto()
    DEL_VAR = auto()
    DUP = auto()
    END_FINALLY = auto()
    FAUX_PUSH = auto()
    FOR_ITER = auto()
    FORMAT = auto()
    GET_ATTR = auto()
    GET_GLOBAL = auto()
    GET_ITEM = auto()
    GET_VAR = auto()
    GET_YIELD_FROM_ITER = auto()
    IMPORT_NAME = auto()
    IMPORT_FROM = auto()
    ITER = auto()
    JUMP = auto()
    JUMP_FORWARD = auto()
    LABEL = auto()
    LAMBDA = auto()
    LOAD_CELL = auto()
    POP = auto()
    POP_BLOCK = auto()
    POP_EXCEPT = auto()
    POP_JUMP_IF_FALSE = auto()
    POP_JUMP_IF_TRUE = auto()
    POSITION = auto()
    RAISE = auto()
    RET_VAL = auto()
    ROT_THREE = auto()
    ROT_TWO = auto()
    SETUP_EXCEPT = auto()
    SETUP_FINALLY = auto()
    SETUP_LOOP = auto()
    SETUP_WITH = auto()
    SET_ATTR = auto()
    SET_GLOBAL = auto()
    SET_ITEM = auto()
    SET_LOCAL = auto()
    SET_VAR = auto()
    UNARY_INVERT = auto()
    UNARY_NEGATIVE = auto()
    UNARY_NOT = auto()
    UNARY_POSITIVE = auto()
    UNPACK_EX = auto()
    UNPACK_SEQUENCE = auto()
    WITH_CLEANUP_FINISH = auto()
    WITH_CLEANUP_START = auto()
    YIELD_FROM = auto()
    YIELD_VAL = auto()


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


def _label_generator(formatstr="label_%04x"):
    counter = 0

    def gen_label():
        nonlocal counter
        counter += 1
        return formatstr % counter

    return gen_label


class CodeBlock(object):

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
                # push(block_i)
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


class PseudopsCompiler(metaclass=ABCMeta):
    """
    Represents a lexical scope, expressions occurring within that
    scope, and nested sub-scopes.
    """

    def __init__(self, parent=None, name=None, args=(), kwonly=0,
                 varargs=False, varkeywords=False,
                 filename=None, declared_at=None,
                 tco_enabled=True, mode=Mode.EXPRESSION):

        self.parent = parent
        self.name = name

        self.mode = mode

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

        # default this feature to True -- instances of the operations
        # CONST and POP immediately in sequence are removed as this is
        # effectively a noisy noop.
        self.compress_const_pop = True


    def __del__(self):
        del self.parent
        del self.blocks
        del self.gen_label
        self.consts.clear()
        del self.consts


    def activate(self, env):
        self.env = env


    def reset(self):
        self.env = None

        if self.blocks:
            self.blocks[0].clear()
        self.blocks = [CodeBlock(Block.BASE, 0, 0)]
        self.consts = [None]

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
        # self.require_active()
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
        block = self._push_block(Block.FINALLY, 0, 6)
        self.pseudop_setup_finally(cleanup_label)
        self.pseudop_debug(" == enter finally ==")
        self.pseudop_label(block.top_label)
        yield block
        self.pseudop_label(block.pop_label)
        self.pseudop_debug(" == exit finally ==")
        self.pseudop_pop_block()
        # self.pseudop_faux_pop(6)
        self._pop_block()


    @contextmanager
    def block_finally_cleanup(self, cleanup_label):
        block = self._push_block(Block.FINALLY_CLEANUP, 6, 1)
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
        block = self._push_block(Block.TRY, 0, 6)
        self.pseudop_setup_except(except_label)
        self.pseudop_debug(" == enter try ==")
        self.pseudop_label(block.top_label)
        yield block
        self.pseudop_label(block.pop_label)
        self.pseudop_debug(" == exit try ==")
        self.pseudop_pop_block()
        # self.pseudop_faux_pop(6)
        self._pop_block()


    @contextmanager
    def block_except(self, except_label):
        block = self._push_block(Block.EXCEPT, 6, 0)
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
        block = self._push_block(Block.EXCEPT_MATCH, 6, 0)
        self.pseudop_label(except_label)
        self.pseudop_debug(" == enter except_match ==")
        self.pseudop_label(block.top_label)
        yield block
        self.pseudop_label(block.pop_label)
        self.pseudop_debug(" == exit except_match ==")
        self.pseudop_pop_except()
        # self.pseudop_faux_pop(4)
        self._pop_block()


    # @contextmanager
    # def block_begin(self):
    #     block = self._push_block(Block.BEGIN, 0, 1)
    #     self.pseudop_debug(" == enter begin ==")
    #     self.pseudop_label(block.top_label)
    #     yield block
    #     self.pseudop_label(block.pop_label)
    #     self.pseudop_debug(" == exit begin ==")
    #     self._pop_block()


    @contextmanager
    def block_with(self, expr):
        cleanup_label = self.gen_label()
        block = self._push_block(Block.WITH, 0, 1)
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
        # self.pseudop_faux_pop(7)
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

        return type(self)(parent=self, **addtl)


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


    def pseudop_load_cell(self, namesym: Symbol):
        self.pseudop(Pseudop.LOAD_CELL, namesym)


    def pseudop_lambda(self, code, *params):
        """
        Pushes a pseudo op to load a lambda from code
        """

        self.declare_const(code)
        self.declare_const(code.co_name)
        self.pseudop(Pseudop.LAMBDA, code, *params)


    def pseudop_pop(self, count=1):
        assert count > 0, ("pseudop_pop with weird count %r" % count)

        if self.compress_const_pop:
            check = Pseudop.CONST
            psops = self.blocks[-1].pseudops

            while count > 0:
                if psops and (psops[-1][0] is check):
                    # this is a POP following a CONST, so let's just
                    # do neither and call it even.
                    psops.pop()
                else:
                    # this POP is meaningful, let's keep it
                    self.pseudop(Pseudop.POP)
                count -= 1

        else:
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
                  _Pseudop.CONTINUE_LOOP,
                  _Pseudop.LOAD_CELL, ):
            push()

        elif op in (_Pseudop.DEL_VAR,
                    _Pseudop.DEL_GLOBAL,
                    _Pseudop.ROT_TWO,
                    _Pseudop.ROT_THREE, ):
            pass

        elif op in (_Pseudop.GET_ATTR,
                    _Pseudop.UNARY_POSITIVE,
                    _Pseudop.UNARY_NEGATIVE,
                    _Pseudop.UNARY_NOT,
                    _Pseudop.UNARY_INVERT,
                    _Pseudop.ITER,
                    _Pseudop.GET_YIELD_FROM_ITER,
                    _Pseudop.YIELD_VAL, ):
            pop()
            push()

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
                    _Pseudop.IMPORT_NAME, ):
            pop(2)
            push()

        elif op in (_Pseudop.SET_ATTR,
                    _Pseudop.DEL_ITEM, ):
            pop(2)

        elif op is _Pseudop.SET_ITEM:
            pop(3)

        elif op in (_Pseudop.SET_GLOBAL,
                    _Pseudop.SET_LOCAL,
                    _Pseudop.SET_VAR,
                    _Pseudop.RET_VAL,
                    _Pseudop.YIELD_FROM,
                    _Pseudop.DEL_ATTR,
                    _Pseudop.POP, ):
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
                    _Pseudop.BUILD_MAP_UNPACK,
                    _Pseudop.BUILD_SLICE,
                    _Pseudop.RAISE, ):
            pop(args[0])
            push()  # we pretend RAISE has a value

        elif op is _Pseudop.SETUP_EXCEPT:
            # push(_Opcode.SETUP_EXCEPT.stack_effect(1))
            push(6)

        elif op is _Pseudop.SETUP_WITH:
            # push(_Opcode.SETUP_WITH.stack_effect(1))
            push(6)

        elif op is _Pseudop.SETUP_FINALLY:
            # push(_Opcode.SETUP_FINALLY.stack_effect(1))
            push(6)

        elif op is _Pseudop.SETUP_LOOP:
            push(_Opcode.SETUP_LOOP.stack_effect(1))

        elif op is _Pseudop.POP_BLOCK:
            push(_Opcode.POP_BLOCK.stack_effect())

        elif op is _Pseudop.POP_EXCEPT:
            # in 3.5 and 3.6 this claims to be 0, but it's actually -3
            # in 3.7 it starts to accurately represent itself as -3
            # so... screw it, let's make it a hard pop 3

            # push(_Opcode.POP_EXCEPT.stack_effect())
            pop(3)

        elif op is _Pseudop.WITH_CLEANUP_START:
            # push(_Opcode.WITH_CLEANUP_START.stack_effect())
            push(7)

        elif op is _Pseudop.WITH_CLEANUP_FINISH:
            # push(_Opcode.WITH_CLEANUP_FINISH.stack_effect())
            pop(7)

        elif op is _Pseudop.END_FINALLY:
            # push(_Opcode.END_FINALLY.stack_effect())
            pop(6)

        elif op is _Pseudop.FAUX_PUSH:
            push(args[0])

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


def _list_unique_append(onto_list, value):

    if value in [0, 1]:
        # we have to manually loop and use the `is` operator, because the
        # list.index method will match False with 0 and True with 1, which
        # incorrectly collapses const pools when both values are present

        for index, found in enumerate(onto_list):
            if found is value:
                # print("found! %r at %i" % (value, index))
                return index
        else:
            onto_list.append(value)
            index = len(onto_list) - 1
            # print("appended! %r at %i" % (value, index))
            return index

    try:
        index = onto_list.index(value)
        # print("found %r at %i" % (value, index))
    except ValueError:
        onto_list.append(value)
        index = len(onto_list) - 1
        # print("appended %r at %i" % (value, index))

    return index


#
# The end.
