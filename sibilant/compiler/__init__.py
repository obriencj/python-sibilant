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

from abc import ABCMeta, abstractmethod
from contextlib import contextmanager
from enum import Enum
from functools import partial
from io import IOBase
from itertools import count
from platform import python_implementation
from sys import version_info
from types import CodeType

from .. import (
    SibilantException,
    symbol, is_symbol, keyword, is_keyword,
    cons, nil, is_pair, is_proper,
)

from ..parse import (
    SibilantSyntaxError,
    default_reader, source_str,
    source_stream, SourceStream,
)


__all__ = (
    "SpecialSyntaxError", "UnsupportedVersion",
    "Opcode", "Pseudop",
    "CodeSpace", "ExpressionCodeSpace",
    "code_space_for_version",
    "Compiled", "is_compiled",
    "Special", "is_special",
    "Macro", "is_macro",
    "Macrolet", "is_macrolet",
    "Operator", "is_operator",
    "iter_compile",
    "gather_formals", "gather_parameters",
)


_keyword_star = keyword("*")
_keyword_starstar = keyword("**")


COMPILER_DEBUG = False


class CompilerException(Exception):
    pass


class CompilerSyntaxError(SibilantSyntaxError):
    pass


class UnsupportedVersion(SibilantException):
    pass


class Compiled():
    __slots__ = ("__name__", )
    __objname__ = "sibilant-compiled"


    def __init__(self, name):
        self.__name__ = name


    def __call__(self, *args, **kwds):
        msg = "Attempt to call %r as a runtime function" % self
        raise TypeError(msg)


    def __repr__(self):
        return "<%s %s>" % (self.__objname__, self.__name__)


    def compile(self, compiler, source_obj):
        pass


def is_compiled(obj):
    return isinstance(obj, Compiled)


class Special(Compiled):
    __objname__ = "special-form"


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
            position = compiler.position_of(source_obj)
            args, kwargs = simple_parameters(source, position)
            expr = self.expand(*args, **kwargs)

        else:
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
    __objname__ = "macrolet"


    def compile(self, compiler, source_obj, tc=False):
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

    def stack_effect(self, arg=None):
        return dis.stack_effect(self.value, arg)


Opcode = Opcode("Opcode", dis.opmap)


# Python 3.6 has this in the enum module, but I support 3.5 so I'll
# just make my own.
_auto = partial(next, count())


class Pseudop(Enum):
    POP = _auto()
    DUP = _auto()
    ROT_TWO = _auto()
    ROT_THREE = _auto()
    RAISE = _auto()
    CALL = _auto()
    CALL_KW = _auto()
    CALL_VAR = _auto()
    CALL_VAR_KW = _auto()
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
    BINARY_LSHIFT = _auto()
    BINARY_RSHIFT = _auto()
    BINARY_AND = _auto()
    BINARY_XOR = _auto()
    BINARY_OR = _auto()
    BUILD_TUPLE = _auto()
    BUILD_TUPLE_UNPACK = _auto()
    BUILD_MAP = _auto()
    BUILD_MAP_UNPACK = _auto()
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
    BLOCK = _auto()
    FAUX_PUSH = _auto()
    DEBUG_STACK = _auto()


_auto = partial(next, count())


class Block(Enum):
    BASE = _auto()
    BEGIN = _auto()
    LOOP = _auto()
    WITH = _auto()
    WITH_CLEANUP = _auto()
    TRY = _auto()
    EXCEPT = _auto()
    EXCEPT_MATCH = _auto()
    FINALLY = _auto()
    FINALLY_CLEANUP = _auto()


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


    def child(self, block_type, init_stack=0, leftovers=0):
        child_block = type(self)(block_type, init_stack, leftovers)
        self.children.append(child_block)
        self.pseudops.append((Pseudop.BLOCK, child_block))
        return child_block


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
        for op, *args in self.pseudops:

            index += 1
            # print(op, args, stac, maxc)

            if op is _Pseudop.POSITION:
                continue

            elif op is _Pseudop.DEBUG_STACK:
                print(" ".join(map(str, args)),
                      "max:", maxc, "current:", stac)

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

        # print("leaving max_stack()", self.block_type, "with", stac)
        assert (stac == leftovers), ("%i / %i left-over stack items"
                                     % (stac, leftovers))

        return stac, maxc


class CodeSpace(metaclass=ABCMeta):
    """
    Represents a lexical scope, expressions occurring within that
    scope, and nested sub-scopes.
    """

    def __init__(self, args=(),
                 varargs=False, varkeywords=False, proper_varargs=True,
                 parent=None, name=None,
                 filename=None, positions=None, declared_at=None,
                 tco_enabled=True):

        self.env = None
        self.parent = parent
        self.name = name

        self.tco_enabled = tco_enabled
        self.tailcalls = 0

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

        self.varargs = varargs
        self.varkeywords = varkeywords

        self.names = []

        # first const is required -- it'll be None or a doc string and
        # then None
        self.consts = [None]

        self.blocks = [CodeBlock(Block.BASE, 0, 0)]

        self.gen_label = _label_generator()
        self._gen_sym = _label_generator("gensym_" + str(id(self)) + "_%04i")

        if not proper_varargs:
            # if our argument formals are an improper list, then the
            # varargs are expected to be a cons list, not a pythonic
            # tuple, and we'll need to perform a translation step at
            # the beginning of the function.
            self.helper_prep_varargs()


    def gen_pseudops(self):
        assert self.blocks, "empty block stack"
        base = self.blocks[0]
        assert base.block_type is Block.BASE, "incorrect base block type"
        yield from base.gen_pseudops()


    def push_block(self, block_type, init_stack=0, leftovers=0):
        self.require_active()
        assert self.blocks, "no code blocks in stack"

        old = self.blocks[-1]
        block = old.child(block_type, init_stack, leftovers)
        self.blocks.append(block)

        return block


    def pop_block(self):
        return self.blocks.pop()


    @contextmanager
    def block_loop(self, test_label, bottom_label, end_label):
        self.push_block(Block.LOOP, 0, 1)
        self.pseudop_setup_loop(end_label)
        self.pseudop_label(test_label)
        self.pseudop_debug(" == enter loop ==")
        yield self
        self.pseudop_debug(" == exit loop ==")
        self.pseudop_label(bottom_label)
        self.pseudop_pop_block()
        self.pseudop_label(end_label)
        self.pop_block()


    @contextmanager
    def block_finally(self, cleanup_label):
        self.push_block(Block.FINALLY, 0, 0)
        self.pseudop_setup_finally(cleanup_label)
        self.pseudop_debug(" == enter finally ==")
        yield self
        self.pseudop_debug(" == exit finally ==")
        self.pseudop_pop_block()
        self.pseudop_faux_pop(2)
        self.pop_block()


    @contextmanager
    def block_finally_cleanup(self, cleanup_label):
        self.push_block(Block.FINALLY_CLEANUP, 0, 0)
        self.pseudop_const(None)
        self.pseudop_label(cleanup_label)
        self.pseudop_debug(" == enter finally_cleanup ==")
        yield self
        self.pseudop_debug(" == exit finally_cleanup ==")
        self.pseudop_end_finally()
        self.pop_block()


    @contextmanager
    def block_try(self, except_label):
        self.push_block(Block.TRY, 0, 0)
        self.pseudop_setup_except(except_label)
        self.pseudop_debug(" == enter try ==")
        yield self
        self.pseudop_debug(" == exit try ==")
        self.pseudop_pop_block()
        self.pseudop_faux_pop(2)
        self.pop_block()


    @contextmanager
    def block_except(self, except_label):
        self.push_block(Block.EXCEPT, 1, 0)
        self.pseudop_label(except_label)
        self.pseudop_debug(" == enter except ==")
        yield self
        self.pseudop_debug(" == exit except ==")
        self.pseudop_end_finally()
        self.pop_block()


    @contextmanager
    def block_except_match(self, except_label):
        self.push_block(Block.EXCEPT_MATCH, 7, 0)
        self.pseudop_label(except_label)
        self.pseudop_debug(" == enter except_match ==")
        yield self
        self.pseudop_debug(" == exit except_match ==")
        self.pseudop_pop_except()
        self.pop_block()


    @contextmanager
    def block_begin(self):
        self.push_block(Block.BEGIN, 0, 1)
        self.pseudop_debug(" == enter begin ==")
        yield self
        self.pseudop_debug(" == exit begin ==")
        self.pop_block()


    @contextmanager
    def block_with(self, expr):
        cleanup_label = self.gen_label()
        self.push_block(Block.WITH, 0, 0)
        self.add_expression(expr)
        self.pseudop_setup_with(cleanup_label)
        self.pseudop_debug(" == enter with ==")
        yield self
        self.pseudop_debug(" == exit with ==")
        self.pseudop_debug(" == enter with_cleanup ==")
        self.pseudop_pop_block()
        self.pseudop_const(None)
        self.pseudop_label(cleanup_label)
        self.pseudop_with_cleanup_start()
        self.pseudop_with_cleanup_finish()
        self.pseudop_end_finally()
        self.pseudop_debug(" == exit with_cleanup ==")
        self.pseudop_faux_pop(3)
        self.pop_block()


    def declare_tailcall(self):
        assert self.tco_enabled, "declare_tailcall without tco_enabled"
        self.tailcalls += 1


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
            raise CompilerException("compiler code space is not active")


    @contextmanager
    def activate(self, env):
        """
        Sets the __compiler__ attribute temporarily in env, and resets
        it when the context exits.
        """

        self.env = env
        old = env.get("__compiler__", None)
        env["__compiler__"] = self

        try:
            yield self

        finally:
            env["__compiler__"] = old
            if old is None:
                del env["__compiler__"]
            self.env = None


    def child(self, args=(), varargs=False, varkeywords=False,
              name=None, declared_at=None, **addtl):

        """
        Returns a child codespace
        """

        if declared_at is None:
            declared_at = self.declared_at

        if name is None:
            name = "%s.<child>" % self.name

        cs = type(self)(parent=self,
                        args=args,
                        varargs=varargs,
                        varkeywords=varkeywords,
                        name=name,
                        filename=self.filename,
                        positions=self.positions,
                        declared_at=declared_at,
                        **addtl)

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


    def helper_prep_varargs(self):
        # initial step which will convert the pythonic varargs tuple
        # into a proper cons list

        if not self.varargs:
            # nothing to do.
            return

        if self.varkeywords:
            # if it's varargs and also varkeywords, the var will be
            # the second-to-last one.
            offset = -2

        else:
            # if it's a varargs, but not varkeywords, the var will be
            # the last one.
            offset = -1

        varname = self.args[offset]

        if self.declared_at:
            self.pseudop_position(*self.declared_at)

        self.pseudop_get_var("make-proper")
        self.pseudop_get_var(varname)
        self.pseudop_call_var(0, 0)
        self.pseudop_set_var(varname)


    def position_of(self, source):
        try:
            return self.positions[id(source)]
        except KeyError:
            return None


    def dup_position_of(self, source, copy):
        try:
            sp = self.positions
            sp[id(copy)] = sp[id(source)]
        except KeyError:
            pass


    def pseudop(self, *op_args):
        """
        Pushes a pseudo op and arguments into the code
        """

        assert self.blocks, "no blocks on stack"
        self.blocks[-1].pseudops.append(op_args)


    if COMPILER_DEBUG:
        def pseudop_debug(self, *op_args):
            self.pseudop(Pseudop.DEBUG_STACK, *op_args)
    else:
        def pseudop_debug(self, *op_args):
            pass


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


    def pseudop_lambda(self, code, default_count):
        """
        Pushes a pseudo op to load a lambda from code
        """

        self.declare_const(code)
        self.declare_const(code.co_name)
        self.pseudop(Pseudop.LAMBDA, code, default_count)


    def pseudop_pop(self, count=1):
        while count > 0:
            self.pseudop(Pseudop.POP)
            count -= 1


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


    def pseudop_build_map(self, count):
        self.pseudop(Pseudop.BUILD_MAP, count)


    def pseudop_build_map_unpack(self, count):
        self.pseudop(Pseudop.BUILD_MAP_UNPACK, count)


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


    def pseudop_binary_matrix_multiply(self):
        self.pseudop(Pseudop.BINARY_MATRIX_MULTIPLY)


    def pseudop_binary_divide(self):
        self.pseudop(Pseudop.BINARY_TRUE_DIVIDE)


    def pseudop_binary_floor_divide(self):
        self.pseudop(Pseudop.BINARY_FLOOR_DIVIDE)


    def pseudop_binary_power(self):
        self.pseudop(Pseudop.BINARY_POWER)


    def pseudop_binary_modulo(self):
        self.pseudop(Pseudop.BINARY_MODULO)


    def pseudop_binary_lshift(self):
        self.pseudop(Pseudop.BINARY_LSHIFT)


    def pseudop_binary_rshift(self):
        self.pseudop(Pseudop.BINARY_RSHIFT)


    def pseudop_binary_and(self):
        self.pseudop(Pseudop.BINARY_AND)


    def pseudop_binary_xor(self):
        self.pseudop(Pseudop.BINARY_XOR)


    def pseudop_binary_or(self):
        self.pseudop(Pseudop.BINARY_OR)


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

        stacksize = self.max_stack()

        flags = CodeFlag.OPTIMIZED.value | CodeFlag.NEWLOCALS.value

        if self.varargs:
            argcount -= 1
            flags |= CodeFlag.VARARGS.value

        if self.varkeywords:
            argcount -= 1
            flags |= CodeFlag.VARKEYWORDS.value

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
                self.pseudop_get_var("nil")
                expr = None

            elif is_pair(expr):
                try:
                    expr = self.compile_pair(expr, tc)
                except TypeError:
                    print(expr)
                    raise

            elif is_symbol(expr):
                try:
                    expr = self.compile_symbol(expr, tc)
                except TypeError:
                    print(expr)
                    raise

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
            raise self.error("cannot evaluate improper lists as expressions",
                             expr)

        position = self.position_of(expr)
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

        # --- new ---

        self.helper_tailcall_tos(tc)
        self.helper_compile_call(tail, position)

        # --- old ---
        # for cl in tail.unpack():
        #     self.add_expression(cl)
        #
        # self.pseudop_position_of(expr)
        # self.pseudop_call(tail.count())

        return None


    def helper_tailcall_tos(self, tc):
        """
        Should be invoked upon TOS functions objects that could
        potentially recur. tc indicates whether the TOS is in a valid
        tailcall position.
        """

        if self.tco_enabled and tc:
            self.pseudop_get_var("tailcall")
            self.pseudop_rot_two()
            self.pseudop_call(1)
            self.declare_tailcall()


    @abstractmethod
    def helper_compile_call(self, args):
        """
        The function to be called is presumed to already be on the stack
        before this is helper is invoked. The helper should assemble the
        arguments as necessary on the stack, and then push the pseudops
        to evaluate them and finally to call the function.
        """

        pass


    def compile_symbol(self, sym, tc=False):
        """
        The various ways that a symbol on its own can evaluate.
        """

        self.require_active()

        comp = self.find_compiled(sym)
        if comp and is_macrolet(comp):
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
                return self.pseudop_get_var(str(sym))
            else:
                return cons(_symbol_attr, *ex, nil)


    def compile_keyword(self, kwd):
        # it should be fairly rare that a keyword is actually
        # passed anywhere at runtime -- it's mostly meant for use
        # as a marker in source expressions for specials.

        self.pseudop_get_var("keyword")
        self.pseudop_const(str(kwd))
        self.pseudop_call(1)
        return None


    def error(self, message, source):
        return CompilerSyntaxError(message, self.position_of(source),
                                   filename=self.filename)


    def find_compiled(self, namesym):
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


    def helper_max_stack(self, op, args, push, pop):

        _Pseudop = Pseudop
        _Opcode = Opcode

        if op is _Pseudop.CONST:
            push()

        elif op in (_Pseudop.GET_VAR,
                    _Pseudop.GET_GLOBAL):
            push()

        elif op is _Pseudop.SET_VAR:
            pop()

        elif op is _Pseudop.DELETE_VAR:
            pass

        elif op in (_Pseudop.GET_ATTR,
                    _Pseudop.UNARY_POSITIVE,
                    _Pseudop.UNARY_NEGATIVE,
                    _Pseudop.UNARY_NOT,
                    _Pseudop.UNARY_INVERT,
                    _Pseudop.ITER):
            pop()
            push()

        elif op is _Pseudop.SET_ATTR:
            pop(2)

        elif op is _Pseudop.DUP:
            push()

        elif op in (_Pseudop.DEFINE_GLOBAL,
                    _Pseudop.DEFINE_LOCAL):
            pop()

        elif op is _Pseudop.POP:
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

        elif op is _Pseudop.RET_VAL:
            pop()

        elif op is _Pseudop.BUILD_MAP:
            pop(args[0] * 2)
            push()

        elif op in (_Pseudop.BUILD_TUPLE,
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

        elif op in (_Pseudop.POP_BLOCK,
                    _Pseudop.POP_EXCEPT):

            pop(4)

        elif op is _Pseudop.WITH_CLEANUP_START:
            push(_Opcode.WITH_CLEANUP_START.stack_effect())

        elif op is _Pseudop.WITH_CLEANUP_FINISH:
            push(_Opcode.WITH_CLEANUP_FINISH.stack_effect())

        elif op is _Pseudop.END_FINALLY:
            pop(1)

        elif op in (_Pseudop.COMPARE_OP,
                    _Pseudop.ITEM,
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
                    _Pseudop.BINARY_OR, ):
            pop(2)
            push()

        elif op is _Pseudop.RAISE:
            pop(args[0])
            # for the sake of counting, let's just pretend that raise
            # evaluates to something.
            push()

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


def iter_compile(source, env, filename=None, reader=None,
                 **codespace_args):

    if isinstance(source, str):
        source = source_str(source, filename)

    elif isinstance(source, IOBase):
        source = source_stream(source, filename)

    if not isinstance(source, SourceStream):
        raise CompilerException("iter_compile source must be str, stream,"
                                " or SourceStream")

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
        codespace = factory(filename=filename, positions=positions,
                            **codespace_args)
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


def gather_formals(args, declared_at=None):
    """
    parses formals pair args into five values:
    (positional, keywords, defaults, stararg, starstararg)

    - positional is a list of symbols defining positional arguments
    - keywords is a list of keywords defining keyword arguments
    - defaults is a list of expressions defining default values for keywords
    - stararg is a symbol for variadic positional arguments
    - starstararg is a symbol for variadic keyword arguments
    """

    def err(msg):
        return SibilantSyntaxError(msg, declared_at)

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

    keywords = []
    defaults = []

    while arg not in (_keyword_star, _keyword_starstar):
        keywords.append(arg)
        defaults.append(next(iargs))

        arg = next(iargs, None)

        if is_keyword(arg):
            continue
        elif arg is None:
            break
        else:
            raise err("keyword formals must be alternating keywords and"
                      " values, not %r" % arg)

    star = None
    starstar = None

    if arg is None:
        return (positional, keywords, defaults, None, None)

    if arg is _keyword_star:
        star = next(iargs, None)
        if not is_symbol(star):
            raise err("* keyword requires symbol binding, not %r" % star)
        arg = next(iargs, None)

    if arg is _keyword_starstar:
        starstar = next(iargs, None)
        if not is_symbol(starstar):
            raise err("** keyword requires symbol binding, not %r" % star)
        arg = next(iargs, None)

    if arg:
        raise err(("leftover formals %r" % arg))

    return (positional, keywords, defaults, star, starstar)


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


def gather_parameters(args, declared_at=None):
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
        return SibilantSyntaxError(msg, declared_at)

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
        raise err("parameters must be symbol or pair, not %r" % args)

    positional = []

    iargs = iter(args.unpack())
    for arg in iargs:
        if is_keyword(arg):
            break
        else:
            positional.append(arg)
    else:
        # handled all if args, done deal.
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

    if arg is None:
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


#
# The end.
