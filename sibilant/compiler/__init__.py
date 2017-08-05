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
from functools import partial, wraps
from itertools import count
from platform import python_implementation
from sys import version_info
from types import CodeType

from .. import (
    nil, symbol, is_pair, is_proper, is_symbol,
    cons, cdr, is_nil,
    SibilantException,
)

from ..ast import (
    SibilantSyntaxError,
    compose_from_str, compose_from_stream,
    compose_all_from_str, compose_all_from_stream,
)


__all__ = (
    "SpecialSyntaxError", "UnsupportedVersion",
    "Opcode", "Pseudop",
    "CodeSpace", "SpecialCodeSpace",
    "code_space_for_version",
    "macro", "is_macro",
    "compile_from_str", "compile_from_stream", "compile_from_ast",
)


class SpecialSyntaxError(SibilantSyntaxError):
    def __init__(self, message, location):
        self.message = message
        if location:
            self.lineno, self.offset = location


class UnsupportedVersion(SibilantException):
    pass


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
    DEFINE = _auto()
    JUMP = _auto()
    JUMP_FORWARD = _auto()
    POP_JUMP_IF_TRUE = _auto()
    POP_JUMP_IF_FALSE = _auto()
    BUILD_TUPLE = _auto()
    BUILD_TUPLE_UNPACK = _auto()
    SETUP_LOOP = _auto()
    SETUP_EXCEPT = _auto()
    SETUP_FINALLY = _auto()
    POP_BLOCK = _auto()
    POP_EXCEPT = _auto()
    EXCEPTION_MATCH = _auto()
    END_FINALLY = _auto()
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
_symbol_getf = symbol("getf")
_symbol_setf = symbol("setf")
_symbol_set_var = symbol("set-var")
_symbol_define = symbol("define")
_symbol_defun = symbol("defun")
_symbol_defmacro = symbol("defmacro")
_symbol_quote = symbol("quote")
_symbol_quasiquote = symbol("quasiquote")
_symbol_unquote = symbol("unquote")
_symbol_splice = symbol("unquote-splicing")
_symbol_begin = symbol("begin")
_symbol_cond = symbol("cond")
_symbol_lambda = symbol("lambda")
_symbol_let = symbol("let")
_symbol_while = symbol("while")
_symbol_raise = symbol("raise")
_symbol_try = symbol("try")
_symbol_else = symbol("else")
_symbol_finally = symbol("finally")


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

        if varargs:
            self._prep_varargs()


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
        return _list_unique_append(self.consts, value)


    def declare_var(self, name):
        name = str(name)
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


    def pseudop_define(self, name):
        """
        Pushes a pseudo op to globally define TOS to name
        """
        _list_unique_append(self.global_vars, name)
        _list_unique_append(self.names, name)
        self.pseudop(Pseudop.DEFINE, name)


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


    def pseudop_exception_match(self):
        self.pseudop(Pseudop.EXCEPTION_MATCH)


    def pseudop_raise(self, count):
        self.pseudop(Pseudop.RAISE, count)
        self.pseudop(Pseudop.FAUX_PUSH, 1)


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
        nlocals = len(self.fast_vars) + len(self.cell_vars)

        stacksize = max_stack(self.pseudops)

        flags = CodeFlag.NEWLOCALS.value | CodeFlag.NESTED.value
        if self.varargs:
            argcount -= 1
            flags |= CodeFlag.VARARGS.value

        if not self.free_vars:
            flags |= CodeFlag.NOFREE.value

        lnt = []
        code = self.code_bytes(lnt)

        consts = tuple(self.consts)

        names = tuple(self.names)

        varnames = list(self.fast_vars)
        for v in self.cell_vars:
            _list_unique_append(varnames, v)
        varnames = tuple(varnames)
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

    def special(namesym):

        def deco(fun):
            _specials[namesym] = fun
            return fun

        return deco

    return special, _specials.items


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


class SpecialCodeSpace(CodeSpace):
    """
    Adds special forms to the basic functionality of CodeSpace
    """

    # decorator and lookup function for built-in special forms
    special, all_specials = _special()


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
            if expr is nil or expr is _symbol_nil:
                return self.pseudop_get_var("nil")

            elif is_pair(expr):
                orig = expr
                head, tail = expr

                if is_symbol(head):
                    # see if this is a special, either a builtin one
                    # or a defined macro.
                    special = self.find_special(head)
                    if special:
                        expr = special.special(self.env, expr)
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
                ex = expr.rsplit(".", 1)
                if len(ex) == 1:
                    return self.pseudop_get_var(str(expr))
                else:
                    expr = cons(_symbol_getf, *ex, nil)
                    continue

            else:
                # TODO there are some literal types that can't be used
                # as constants, will need to fill those in here. For
                # now we're just presuming it's going to be a
                # constant, the end.
                return self.pseudop_const(expr)


    def add_expression_with_pop(self, expr):
        """
        Insert an expression, then an op to pop its result off of the
        stack
        """

        self.add_expression(expr)
        self.pseudop_pop()


    def add_expression_with_return(self, expr):
        """
        Insert an expression, then an op to return its result from the
        current call
        """
        self.add_expression(expr)
        self.pseudop_return()


    def error(self, message, source):
        return SpecialSyntaxError(message, self.position_of(source))


    @special(_symbol_doc)
    def special_doc(self, source):
        called_by, rest = source

        self.set_doc(" ".join(d.strip() for d in map(str, rest.unpack())))

        # doc special expression evaluates to None
        self.pseudop_const(None)

        return None


    @special(_symbol_getf)
    def special_getf(self, source):
        try:
            called_by, (obj, (member, rest)) = source
        except ValueError:
            raise self.error("too few arguments to getf", source)

        if not is_nil(rest):
            raise self.error("too many arguments to getf", source)

        self.pseudop_position_of(source)
        self.add_expression(obj)
        self.pseudop_getattr(str(member))

        # no further transformations
        return None


    @special(_symbol_setf)
    def special_setf(self, source):
        try:
            called_by, (obj, (member, (value, rest))) = source
        except ValueError:
            raise self.error("too few arguments to setf", source)

        if not is_nil(rest):
            raise self.error("too many arguments to setf", source)

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
        if not body:
            # because all things are expressions, an empty begin still
            # needs to have a return value. In this case, the return value
            # will by the python None
            return self.pseudop_const(None)

        self.pseudop_position_of(body)

        # interleave pops with expr, except for the last one
        first = True
        for expr in body.unpack():
            if first:
                first = False
            else:
                self.pseudop_pop()
            self.add_expression(expr)


    @special(_symbol_lambda)
    def special_lambda(self, source):
        """
        Special form for lambda
        """

        called_by, (args, body) = source

        if is_symbol(args):
            pyargs = [str(args)]
            varargs = True

        elif is_pair(args):
            varargs = not is_proper(args)
            pyargs = []
            for arg in args.unpack():
                if not is_symbol(arg):
                    raise self.error("formals must be symbols", cdr(source))
                else:
                    pyargs.append(str(arg))

        else:
            msg = "formals must be symbol or pair, not %r" % type(args)
            raise self.error(msg, cdr(source))

        kid = self.child_context(args=pyargs, varargs=varargs,
                                 name="<lambda>",
                                 declared_at=self.position_of(source))

        with kid as subc:
            subc.helper_begin(body)
            subc.pseudop_return()
            code = subc.complete()

        self.pseudop_lambda(code)

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

        kid = self.child_context(args=args, name="<let>",
                                 declared_at=self.position_of(source))

        with kid as subc:
            subc.helper_begin(body)
            subc.pseudop_return()
            code = subc.complete()

        self.pseudop_lambda(code)

        for val in vals:
            self.add_expression(val)

        self.pseudop_call(len(vals))

        # no additional transform needed
        return None


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


    # @special(symbol("while"))
    def special_new_while(self, source):

        called_by, (test, body) = source

        looptop = self.gen_label()
        loopbottom = self.gen_label()
        eoloop = self.gen_label()

        self.pseudop_const(None)
        self.pseudop_setup_loop(eoloop)
        self.pseudop_label(looptop)
        self.add_expression(test)
        self.pseudop_pop_jump_if_false(loopbottom)

        # try
        # except continue
        # except break

        self.pseudop_jump(looptop)
        self.pseudop_label(loopbottom)
        self.pseudop_pop_block()
        self.pseudop_label(eoloop)

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

        label_end = self.gen_label()
        label_next = self.gen_label()

        if has_finally:
            # setup that finally block if we need one
            self.pseudop_setup_finally(label_finally)

        if normal_catches:
            # we don't setup an actual try block if there's no
            # catching going on, it breaky things.
            self.pseudop_setup_except(label_next)

        # Here's the expression we're actually wrapping in a try
        self.add_expression(expr)

        if has_else:
            # the result of the expression is thrown away if we have
            # an else clause
            self.pseudop_pop()

            if normal_catches:
                # if there were no normal_catches found, there's no
                # block to pop
                self.pseudop_pop_block()

            # and we'll need to jump ahead to the else code.
            self.pseudop_jump_forward(label_else)

        else:
            # but if there isn't an else clause, then the result of
            # the expression is the real deal
            self.pseudop_return()

            if normal_catches:
                self.pseudop_pop_block()

            self.pseudop_jump_forward(label_end)

        if normal_catches:
            # each attempt to match the exception should have us at
            # the TOS,TOS-1,TOS-2 setup
            self.pseudop_faux_push(3)

            # TOS exception type, TOS-1 exception, TOS-2 backtrace
            for ca in normal_catches:
                ex, act = ca

                self.pseudop_position_of(ca)
                self.pseudop_label(label_next)
                label_next = self.gen_label()

                self.pseudop_debug("start of handler for", ex)

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

                    self.declare_var(key)

                    cleanup = self.gen_label()

                    self.pseudop_dup()
                    self.add_expression(match)
                    self.pseudop_exception_match()
                    self.pseudop_pop_jump_if_false(label_next)
                    self.pseudop_pop()
                    self.pseudop_set_var(str(key))
                    self.pseudop_pop()

                    self.pseudop_setup_finally(cleanup)
                    self.pseudop_debug("before body of handler for", ex)
                    self.helper_begin(act)
                    self.pseudop_return()
                    self.pseudop_pop_block()
                    self.pseudop_debug("end of handler for", ex)

                    if (3, 6) <= version_info:
                        pass

                    elif (3, 5) <= version_info < (3, 6):
                        self.pseudop_pop_except()
                        self.pseudop_const(None)
                        self.pseudop_faux_pop(1)

                    self.pseudop_label(cleanup)
                    self.pseudop_const(None)
                    self.pseudop_set_var(str(key))
                    self.pseudop_del_var(str(key))
                    self.pseudop_end_finally()
                    self.pseudop_jump_forward(label_end)

                else:
                    # an exception match without a binding

                    self.pseudop_dup()
                    self.add_expression(ex)
                    self.pseudop_exception_match()
                    self.pseudop_pop_jump_if_false(label_next)
                    self.pseudop_pop()
                    self.pseudop_pop()
                    self.pseudop_pop()

                    self.pseudop_debug("before body of handler for", ex)
                    self.helper_begin(act)
                    self.pseudop_return()
                    self.pseudop_debug("end of handler for", ex)

                    if (3, 6) <= version_info:
                        pass

                    elif (3, 5) <= version_info < (3, 6):
                        self.pseudop_pop_except()
                        self.pseudop_const(None)
                        self.pseudop_faux_pop(1)
                        self.pseudop_jump_forward(label_end)

            # after all the attempts at trying to match the exception,
            # we land here.
            self.pseudop_label(label_next)
            self.pseudop_faux_pop(3)
            self.pseudop_end_finally()

        if has_else:
            # okay, we've arrived at the else handler, run it, and
            # return that value.
            self.pseudop_label(label_else)
            self.helper_begin(act_else)
            self.pseudop_return()

        self.pseudop_label(label_end)

        if has_finally:
            # first we have to cleanup, popping the finally block,
            # which will trigger the jump to the label_finally
            self.pseudop_pop_block()
            self.pseudop_const(None)
            self.pseudop_faux_pop(1)

            # here's the actual handling of the finally event
            self.pseudop_label(label_finally)
            self.helper_begin(act_finally)
            self.pseudop_return()

            if (3, 6) <= version_info:
                pass
            elif (3, 5) <= version_info < (3, 6):
                self.pseudop_end_finally()

        else:
            # guess we're all done, actually.
            self.pseudop_return_none()

        # no further transformations needed on the passed source code.
        return None


    @special(_symbol_set_var)
    def special_set_var(self, source):

        called_by, (binding, body) = source

        if not is_symbol(binding):
            raise self.error("assignment must be by symbolic name",
                             cdr(source))

        value, rest = body
        if not is_nil(rest):
            raise self.error("extra values in assignment", rest)

        if not is_pair(value):
            self.pseudop_position_of(body)

        self.add_expression(value)
        self.pseudop_set_var(str(binding))

        # set-var calls should evaluate to None
        self.pseudop_const(None)

        # no additional transform needed
        return None


    @special(_symbol_define)
    def special_define(self, source):

        called_by, (binding, body) = source

        self.helper_begin(body)

        if is_symbol(binding):
            self.pseudop_define(str(binding))
        else:
            assert(False)

        # define expression evaluates to None
        self.pseudop_const(None)

        return None


    @special(_symbol_defun)
    def special_defun(self, source):

        called_by, (namesym, cl) = source
        name = str(namesym)

        args, body = cl

        if is_symbol(args):
            args = [str(args)]
            varargs = True

        elif is_pair(args):
            varargs = not is_proper(args)
            args = map(str, args.unpack())

        else:
            msg = "formals must be symbol or pair, not %r" % type(args)
            raise self.error(msg, cl)

        kid = self.child_context(args=args, varargs=varargs,
                                 name=name,
                                 declared_at=self.position_of(source))

        with kid as subc:
            subc.helper_begin(body)
            subc.pseudop_return()
            code = subc.complete()

        self.pseudop_lambda(code)
        self.pseudop_define(name)

        # defun expression evaluates to None
        self.pseudop_const(None)

        # no additional transform needed
        return None


    @special(_symbol_defmacro)
    def special_defmacro(self, source):

        called_by, (namesym, cl) = source
        name = str(namesym)

        args, body = cl

        if is_symbol(args):
            args = [str(args)]
            varargs = True

        elif is_pair(args):
            varargs = not is_proper(args)
            args = map(str, args.unpack())

        else:
            msg = "formals must be symbol or pair, not %r" % type(args)
            raise self.error(msg, cl)

        kid = self.child_context(args=args, varargs=varargs,
                                 name=name,
                                 declared_at=self.position_of(source))

        with kid as subc:
            subc.helper_begin(body)
            subc.pseudop_return()
            code = subc.complete()

        self.pseudop_get_var("macro")
        self.pseudop_lambda(code)
        self.pseudop_call(1)

        self.pseudop_define(name)

        # defmacro expression evaluates to None
        self.pseudop_const(None)

        # no additional transform needed
        return None


    @special(_symbol_cond)
    def special_cond(self, source):

        called_by, cl = source

        self.pseudop_label(self.gen_label())

        done = self.gen_label()
        label = self.gen_label()

        for test, body in cl.unpack():
            self.pseudop_label(label)
            label = self.gen_label()

            if test is _symbol_else:
                self.helper_begin(body)
                self.pseudop_jump(done)
                break

            else:
                self.add_expression(test)
                self.pseudop_pop_jump_if_false(label)
                self.helper_begin(body)
                self.pseudop_jump(done)

        self.pseudop_label(label)
        self.pseudop_const(None)

        self.pseudop_label(done)


    def find_special(self, namesym):
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

        if found and is_special(found):
            # we found a Macro instance, return the relevant
            # method
            return found
        else:
            # what we found doesn't qualify, throw it away
            return None


def _list_unique_append(l, v):
    if v in l:
        return l.index(v)
    else:
        l.append(v)
        return len(l)


def max_stack(pseudops):
    """
    Calculates the maximum stack size from the pseudo operations. This
    function is total crap, but it's good enough for now.
    """

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
        assert(stac >= 0)

    # print("max_stack()")
    for op, *args in pseudops:
        # print(op, args, stac, maxc)

        if op is Pseudop.POSITION:
            pass

        elif op is Pseudop.DEBUG_STACK:
            print(" ".join(map(str, args)), "max:", maxc, "current:", stac)

        elif op is Pseudop.CALL:
            pop(args[0])

        elif op is Pseudop.CONST:
            push()

        elif op is Pseudop.GET_VAR:
            push()

        elif op is Pseudop.SET_VAR:
            pop()

        elif op is Pseudop.DELETE_VAR:
            pass

        elif op is Pseudop.GET_ATTR:
            pop()
            push()

        elif op is Pseudop.SET_ATTR:
            pop(2)

        elif op is Pseudop.DUP:
            push()

        elif op is Pseudop.DEFINE:
            pop()

        elif op is Pseudop.POP:
            pop()

        elif op is Pseudop.LAMBDA:
            a = len(args[0].co_freevars)
            if a:
                push(a)
                pop(a)
            push(2)
            pop(2)
            push()

        elif op is Pseudop.RET_VAL:
            pop()

        elif op in (Pseudop.JUMP,
                    Pseudop.JUMP_FORWARD):
            at_label[args[0]] = stac

        elif op is Pseudop.LABEL:
            stac = at_label.get(args[0], stac)

        elif op in (Pseudop.POP_JUMP_IF_TRUE,
                    Pseudop.POP_JUMP_IF_FALSE):
            pop()
            at_label[args[0]] = stac

        elif op is Pseudop.CALL_VARARGS:
            # TODO: need to revamp CALL_VARARGS to act on an optional
            # kwargs as well
            pop()

        elif op in (Pseudop.BUILD_TUPLE,
                    Pseudop.BUILD_TUPLE_UNPACK):
            pop(args[0])
            push()

        elif op in (Pseudop.SETUP_EXCEPT,
                    Pseudop.SETUP_FINALLY,
                    Pseudop.POP_BLOCK,
                    Pseudop.POP_EXCEPT,
                    Pseudop.END_FINALLY):
            pass

        elif op is Pseudop.EXCEPTION_MATCH:
            pop(2)
            push()

        elif op is Pseudop.RAISE:
            pop(args[0])

        elif op is Pseudop.FAUX_PUSH:
            push(args[0])

        elif op in (Pseudop.ROT_THREE,
                    Pseudop.ROT_TWO):
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


class Special(object):
    def __init__(self, fun, name=None):
        self.special = fun
        self.__name__ = name or fun.__name__

    def __call__(self, *args, **kwds):
        t = type(self)
        n = self.__name__
        msg = "Attempt to call %s %s as runtime function." % (t, n)
        raise TypeError(msg)


def special(fun):
    if is_special(fun):
        return fun
    else:
        return Special(fun)


def is_special(value):
    return isinstance(value, Special)


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


class Macro(Special):
    def __init__(self, fun, name=None):
        self.expand = fun
        self.__name__ = name or fun.__name__

    def special(self, _env, source):
        called_by, cl = source
        return self.expand(*cl.unpack())


def macro(fun):
    if is_macro(fun):
        return fun
    else:
        return Macro(fun)


def is_macro(value):
    return isinstance(value, Macro)


def builtin_specials():
    def gen_wrapper(meth):
        @wraps(meth)
        def invoke_special(env, cl):
            compiler = env.get("__compiler__", None)
            if not compiler:
                raise Exception("special invoked without active compiler")
            return meth(compiler, cl)

        return invoke_special

    for sym, meth in SpecialCodeSpace.all_specials():
        yield str(sym), Special(gen_wrapper(meth), name=sym)


def compile_from_ast(astree, env, filename=None):
    positions = {}

    cl = astree.simplify(positions)

    factory = code_space_for_version(version_info)
    if not factory:
        raise UnsupportedVersion(version_info)

    codespace = factory(filename=filename, positions=positions)

    with codespace.activate(env):
        assert(env.get("__compiler__") == codespace)
        codespace.add_expression_with_return(cl)
        code = codespace.complete()

    return code


def compile_from_stream(stream, env, filename=None):
    astree = compose_from_stream(stream)
    return compile_from_ast(astree, env)


def compile_from_str(src_str, env, filename=None):
    astree = compose_from_str(src_str)
    return compile_from_ast(astree, env)


def compile_all_from_stream(stream, env):
    for astree in compose_all_from_stream(stream):
        yield compile_from_ast(astree, env)


def compile_all_from_str(src_str, env):
    for astree in compose_all_from_str(src_str):
        yield compile_from_ast(astree, env)


#
# The end.
