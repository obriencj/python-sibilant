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

from contextlib import contextmanager
from enum import Enum
from functools import partial, wraps
from itertools import count
from sys import version_info
from types import CodeType

from . import (
    nil, symbol, is_pair, is_proper, is_symbol,
    cons, is_nil,
    SibilantException,
)

from .ast import (
    compose_from_str, compose_from_stream,
    compose_all_from_str, compose_all_from_stream,
)


__all__ = (
    "UnsupportedVersion",
    "Opcode", "Pseudop",
    "CodeSpace", "SpecialsCodeSpace",
    "macro", "is_macro",
    "compile_from_str", "compile_from_stream", "compile_from_ast",
)


class SyntaxError(SibilantException):
    pass


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


class CodeSpace(object):
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

        # first const is required -- it'll be None or a doc string
        self.consts = [None]

        self.pseudops = []

        self.gen_label = label_generator()

        if varargs:
            self._prep_varargs()


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

        elif name in self.fast_vars:
            # we need to convert this fast var into a cell var for our
            # child namespace to use
            self.fast_vars.remove(name)
            _list_unique_append(self.cell_vars, name)
            return True

        elif ((name in self.free_vars) or
              (name in self.cell_vars)):
            # yup, we can provide that cell
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


    def pseudop(self, *op_args):
        """
        Pushes a pseudo op and arguments into the code
        """
        self.pseudops.append(op_args)


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

        # print("in complete, lnt is:", lnt)
        # print("self.positions is:", self.positions)

        consts = tuple(self.consts)
        names = tuple(self.names)
        varnames = *self.fast_vars, *self.cell_vars
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


    def code_bytes(self, lnt):
        offset = 0
        coll = []

        labels = {}

        def add_label(name):
            labels[name] = offset

        jabs = []

        def mark_jabs(label_name):
            jabs.append((len(coll), label_name))

        jrel = []

        def mark_jrel(label_name):
            jrel.append((len(coll), label_name, offset))

        def set_position(line, col):
            lnt.append((offset, line, col))

        if (3, 6) <= version_info:
            for op, *args in self._gen_code(add_label, set_position):
                if op.hasjabs():
                    # deal with jumps, so we can set their argument
                    # to an appropriate label offset later

                    assert(args)
                    mark_jabs(args[0])
                    # we are being lazy here, and padding out our
                    # jumps with an EXTENDED_ARG, in case we need more
                    # than 8 bits of address once labels are applied.
                    coll.append([Opcode.EXTENDED_ARG, 0])
                    coll.append([op, 0])
                    offset += 4

                elif op.hasjrel():
                    # relative jump!

                    assert(args)
                    mark_jrel(args[0])
                    coll.append([Opcode.EXTENDED_ARG, 0])
                    coll.append([op, 0])
                    offset += 4

                else:
                    # filter so that all opcodes have exactly one
                    # argument
                    coll.append((op, args[0] if args else 0))
                    offset += 2

        elif (3, 3) <= version_info < (3, 6):
            for op, *args in self._gen_code(add_label, set_position):

                if op.hasjabs():
                    # deal with jumps, so we can set their argument to
                    # an appropriate label offset later

                    assert(args)
                    mark_jabs(args[0])
                    coll.append([op, 0, 0])
                    offset += 3

                elif op.hasjrel():
                    # relative jump!

                    assert(args)
                    mark_jrel(args[0])
                    coll.append([op, 0, 0])
                    offset += 3

                else:
                    coll.append((op, *args))
                    offset += (1 + len(args))

        else:
            raise UnsupportedVersion(version_info)

        if jabs or jrel or labels:
            # Given our labels, modify jmp calls to point to the label
            apply_jump_labels(coll, jabs, jrel, labels)

        coll = ((c.value, *a) for c, *a in coll)
        return b''.join(bytes(c) for c in coll)


    def _gen_code(self, declare_label, declare_position):
        """
        Given the pseudo operations added to this CodeSpace, the named
        variables declared and requested, yield the CPython opcodes
        and arguments to represent this code.
        """

        # print("gen_code()")
        # for op, *args in self.pseudops:
        #     print(op, args)

        for op, *args in self.pseudops:
            if op is Pseudop.POSITION:
                declare_position(*args)

            elif op is Pseudop.CALL:
                n = args[0]
                yield Opcode.CALL_FUNCTION, n, 0

            elif op is Pseudop.CONST:
                i = self.consts.index(*args)
                yield Opcode.LOAD_CONST, i, 0

            elif op is Pseudop.GET_VAR:
                n = args[0]
                if n in self.fast_vars:
                    i = self.fast_vars.index(n)
                    yield Opcode.LOAD_FAST, i, 0
                elif n in self.cell_vars:
                    i = self.cell_vars.index(n)
                    yield Opcode.LOAD_DEREF, i, 0
                elif n in self.free_vars:
                    i = self.free_vars.index(n) + len(self.cell_vars)
                    yield Opcode.LOAD_DEREF, i, 0
                elif n in self.global_vars:
                    i = self.names.index(n)
                    yield Opcode.LOAD_GLOBAL, i, 0
                else:
                    assert(False)

            elif op is Pseudop.SET_VAR:
                n = args[0]
                if n in self.fast_vars:
                    i = self.fast_vars.index(n)
                    yield Opcode.STORE_FAST, i, 0
                elif n in self.cell_vars:
                    i = self.cell_vars.index(n)
                    yield Opcode.STORE_DEREF, i, 0
                elif n in self.free_vars:
                    i = self.free_vars.index(n) + len(self.cell_vars)
                    yield Opcode.STORE_DEREF, i, 0
                elif n in self.global_vars:
                    i = self.names.index(n)
                    yield Opcode.STORE_GLOBAL, i, 0
                else:
                    assert(False)

            elif op is Pseudop.GET_ATTR:
                n = args[0]
                i = self.names.index(n)
                yield Opcode.LOAD_ATTR, i, 0

            elif op is Pseudop.SET_ATTR:
                n = args[0]
                i = self.names.index(n)
                yield Opcode.STORE_ATTR, i, 0

            elif op is Pseudop.DEFINE:
                n = args[0]
                if n in self.global_vars:
                    i = self.names.index(n)
                    yield Opcode.STORE_GLOBAL, i, 0
                else:
                    assert(False)

            elif op is Pseudop.POP:
                yield Opcode.POP_TOP,

            elif op is Pseudop.LAMBDA:
                yield from self._gen_lambda(*args)

            elif op is Pseudop.RET_VAL:
                yield Opcode.RETURN_VALUE,

            elif op is Pseudop.DUP:
                yield Opcode.DUP_TOP,

            elif op is Pseudop.LABEL:
                declare_label(args[0])

            elif op is Pseudop.FAUX_PUSH:
                pass

            elif op is Pseudop.JUMP:
                yield Opcode.JUMP_ABSOLUTE, args[0], 0

            elif op is Pseudop.JUMP_FORWARD:
                yield Opcode.JUMP_FORWARD, args[0], 0

            elif op is Pseudop.POP_JUMP_IF_TRUE:
                yield Opcode.POP_JUMP_IF_TRUE, args[0]

            elif op is Pseudop.POP_JUMP_IF_FALSE:
                yield Opcode.POP_JUMP_IF_FALSE, args[0]

            elif op is Pseudop.CALL_VARARGS:
                if (3, 6) <= version_info:
                    yield Opcode.CALL_FUNCTION_EX, 0
                elif (3, 3) <= version_info < (3, 6):
                    yield Opcode.CALL_FUNCTION_VAR, args[0], 0
                else:
                    raise UnsupportedVersion(version_info)

            elif op is Pseudop.BUILD_TUPLE:
                yield Opcode.BUILD_TUPLE, args[0], 0

            elif op is Pseudop.BUILD_TUPLE_UNPACK:
                yield Opcode.BUILD_TUPLE_UNPACK, args[0], 0

            elif op is Pseudop.SETUP_EXCEPT:
                yield Opcode.SETUP_EXCEPT, args[0], 0

            elif op is Pseudop.SETUP_FINALLY:
                yield Opcode.SETUP_FINALLY, args[0], 0

            elif op is Pseudop.POP_BLOCK:
                yield Opcode.POP_BLOCK,

            elif op is Pseudop.POP_EXCEPT:
                yield Opcode.POP_EXCEPT,

            elif op is Pseudop.END_FINALLY:
                yield Opcode.END_FINALLY,

            elif op is Pseudop.EXCEPTION_MATCH:
                yield Opcode.COMPARE_OP, 10, 0

            elif op is Pseudop.RAISE:
                yield Opcode.RAISE_VARARGS, args[0], 0

            elif op is Pseudop.ROT_TWO:
                yield Opcode.ROT_TWO,

            elif op is Pseudop.ROT_THREE:
                yield Opcode.ROT_THREE,

            else:
                assert(False)


    def _gen_lambda(self, code):
        """
        Helper to _gen_code that handles just lambda definitions
        """

        ci = self.consts.index(code)
        ni = self.consts.index(code.co_name)

        if code.co_freevars:
            # code is a closure, so we'll need to find the matching
            # free/cell vars and provide them.

            for f in code.co_freevars:
                if f in self.cell_vars:
                    fi = self.cell_vars.index(f)
                elif f in self.free_vars:
                    fi = len(self.cell_vars)
                    fi += self.free_vars.index(f)
                else:
                    assert(False)
                yield Opcode.LOAD_CLOSURE, fi, 0

            yield Opcode.BUILD_TUPLE, len(code.co_freevars), 0
            yield Opcode.LOAD_CONST, ci, 0
            yield Opcode.LOAD_CONST, ni, 0

            if (3, 6) <= version_info:
                # print("Using MAKE_FUNCTION for 3.6")
                yield Opcode.MAKE_FUNCTION, 0x08, 0
            elif (3, 3) <= version_info < (3, 6):
                # print("Using MAKE_CLOSURE for 3.3+")
                yield Opcode.MAKE_CLOSURE, 0, 0
            else:
                # print("Unsupported:", _PYVER)
                assert(False)

        else:
            # not a closure, so just a pain ol' function
            yield Opcode.LOAD_CONST, ci, 0
            yield Opcode.LOAD_CONST, ni, 0
            yield Opcode.MAKE_FUNCTION, 0, 0


def _special():
    _specials = {}

    def special(namesym):

        def deco(fun):
            _specials[namesym] = fun
            return fun

        return deco

    return special, _specials.items


class SpecialsCodeSpace(CodeSpace):
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
            if expr is nil:
                return self.pseudop_const(nil)

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

            # elif is_pair(expr):
            #     # improper list means method call

            #     obj, rest = expr
            #     if is_pair(rest):
            #         member, args = rest
            #     else:
            #         member = rest
            #         args = nil

            #     if is_symbol(member):
            #         self.add_expression(obj)

            #         self.pseudop_position_of(rest)
            #         self.pseudop_getattr(str(member))

            #         for ex in args.unpack():
            #             self.add_expression(ex)

            #         self.pseudop_position_of(expr)
            #         self.pseudop_call(args.count())

            #         return None

            #     else:
            #         expr = cons(cons(symbol("getattr"), obj,
            #                          cons(symbol("str"), member, nil), nil),
            #                     *expr.unpack(), nil)
            #         continue

            elif is_symbol(expr):
                ex = expr.rsplit(".", 1)
                if len(ex) == 1:
                    return self.pseudop_get_var(str(expr))
                else:
                    expr = cons(symbol("getf"), *ex, nil)
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


    @special(symbol("getf"))
    def special_getf(self, source):
        try:
            called_by, (obj, (member, rest)) = source
        except ValueError:
            raise SyntaxError("too few arguments to getf", source)

        if not is_nil(rest):
            raise SyntaxError("too many arguments to getf", source)

        self.pseudop_position_of(source)
        self.add_expression(obj)
        self.pseudop_getattr(str(member))

        # no further transformations
        return None


    @special(symbol("setf"))
    def special_setf(self, source):
        try:
            called_by, (obj, (member, (value, rest))) = source
        except ValueError:
            raise SyntaxError("too few arguments to setf", source)

        if not is_nil(rest):
            raise SyntaxError("too many arguments to setf", source)

        self.add_expression(obj)
        self.add_expression(value)
        self.pseudop_rot_two()
        self.pseudop_setattr(str(member))

        # make setf calls evaluate to None
        self.pseudop_const(None)

        # no further transformations
        return None


    @special(symbol("quote"))
    def special_quote(self, source):
        """
        Special form for quote
        """

        called_by, body = source

        self.pseudop_position_of(source)
        self.helper_quote(body)

        # no additional transform needed
        return None


    def helper_quote(self, body):
        if body is nil:
            self.pseudop_get_var("nil")

        elif is_symbol(body):
            self.pseudop_get_var("symbol")
            self.pseudop_const(str(body))
            self.pseudop_call(1)

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


    @special(symbol("quasiquote"))
    def special_quasiquote(self, source):
        """
        Special form for quasiquote
        """

        called_by, body = source

        self.pseudop_position_of(source)
        self.helper_quasiquote(body)

        return None


    def helper_quasiquote(self, body):
        if body is nil:
            self.pseudop_get_var("nil")

        elif is_symbol(body):
            self.pseudop_get_var("symbol")
            self.pseudop_const(str(body))
            self.pseudop_call(1)

        elif is_pair(body):
            if is_proper(body):
                self.pseudop_get_var("make-proper")
            else:
                self.pseudop_get_var("cons")

            coll_tup = 0
            curr_tup = 0

            for c in body.unpack():
                if c is nil:
                    coll_tup += 1
                    self.pseudop_get_var("nil")

                elif is_symbol(c):
                    self.pseudop_get_var("symbol")
                    self.pseudop_const(str(c))
                    self.pseudop_call(1)
                    coll_tup += 1

                elif is_pair(c):
                    head, tail = c
                    if head is symbol("unquote"):
                        if is_pair(tail):
                            u_head, u_tail = tail
                            if u_head is symbol("splice"):
                                if coll_tup:
                                    self.pseudop_build_tuple(coll_tup)
                                    coll_tup = 0
                                    curr_tup += 1
                                self.pseudop_get_var("to-tuple")
                                self.add_expression(u_tail)
                                self.pseudop_call(1)
                                curr_tup += 1
                            else:
                                self.add_expression(tail)
                                coll_tup += 1
                        else:
                            self.add_expression(tail)
                            coll_tup += 1
                    else:
                        self.helper_quasiquote(c)
                        coll_tup += 1

                else:
                    self.pseudop_const(c)
                    coll_tup += 1

            if coll_tup:
                self.pseudop_build_tuple(coll_tup)
                coll_tup = 0
                curr_tup += 1

            self.pseudop_build_tuple_unpack(curr_tup)
            self.pseudop_call_varargs(0)


    @special(symbol("begin"))
    def special_begin(self, source):
        """
        Special form for begin
        """

        called_by, body = source

        self.pseudop_position_of(source)
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


    @special(symbol("lambda"))
    def special_lambda(self, source):
        """
        Special form for lambda
        """

        called_by, (args, body) = source

        if is_symbol(args):
            args = [str(args)]
            varargs = True

        elif is_pair(args):
            varargs = not is_proper(args)
            args = map(str, args.unpack())

        else:
            raise SyntaxError("formals must be symbol or pair, not %r" %
                              args)

        try:
            declared_at = self.positions[id(source)]
        except KeyError:
            declared_at = None

        kid = self.child_context(args=args, varargs=varargs,
                                 name="<lambda>",
                                 declared_at=declared_at)

        with kid as subc:
            subc.helper_begin(body)
            subc.pseudop_return()
            code = subc.complete()

        self.pseudop_lambda(code)

        # no additional transform needed
        return None


    @special(symbol("let"))
    def special_let(self, source):

        called_by, (bindings, body) = source

        args = []
        vals = []
        for arg in bindings.unpack():
            name, val = arg.unpack()
            args.append(str(name))
            vals.append(val)

        try:
            declared_at = self.positions[id(source)]
        except KeyError:
            declared_at = None

        kid = self.child_context(args=args, name="<let>",
                                 declared_at=declared_at)

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


    @special(symbol("while"))
    def special_while(self, source):

        called_by, (test, body) = source

        top = self.gen_label()
        done = self.gen_label()

        self.pseudop_label(top)
        self.add_expression(test)

        self.pseudop_pop_jump_if_false(done)
        self.helper_begin(body)
        self.pseudop_pop()
        self.pseudop_jump(top)

        self.pseudop_label(done)
        self.pseudop_const(None)

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


    @special(symbol("raise"))
    def special_raise(self, source):

        called_by, cl = source

        c = cl.count()
        if c > 3:
            raise SyntaxError("too many arguments to raise", cl)

        for rx in cl.unpack():
            self.add_expression(rx)

        self.pseudop_position_of(source)
        self.pseudop_raise(c)

        return None


    @special(symbol("try"))
    def special_try(self, source):

        try:
            declared_at = self.positions[id(source)]
        except KeyError:
            declared_at = None

        kid = self.child_context(name="<try>", declared_at=declared_at)
        with kid as subc:
            subc._helper_special_try(source)
            code = subc.complete()

        self.pseudop_lambda(code)
        self.pseudop_call(0)

        return None


    def _helper_special_try(self, source):

        called_by, (expr, catches) = source

        sym_finally = symbol("finally")
        sym_else = symbol("else")

        has_finally = False
        has_else = False

        normal_catches = []

        # first, filter our catches down into normal exception
        # matches, finally, and else
        for ca in catches.unpack():
            ex, act = ca

            if not act:
                raise SyntaxError("clause with no body in try", ca)

            if ex is sym_finally:
                if has_finally:
                    raise SyntaxError("duplicate finally clause in try", ca)
                has_finally = True
                act_finally = act
                label_finally = self.gen_label()

            elif ex is sym_else:
                if has_else:
                    raise SyntaxError("duplicate else clause in try", ca)
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
            # each attempt to match the exception should have us at
            # the TOS,TOS-1,TOS-2 setup
            self.pseudop_faux_push(3)

            # TOS is our exception, TOS-1 is type, TOS-2 is backtrace
            for ca in normal_catches:
                ex, act = ca

                self.pseudop_position_of(ca)
                self.pseudop_label(label_next)
                label_next = self.gen_label()

                if is_pair(ex):
                    # The exception is intended to be bound to a local
                    # variable. To achieve that, we're going to set up
                    # a lambda with that single binding, and stuff out
                    # handler code inside of it.

                    if not is_proper(ex):
                        raise SyntaxError()

                    match, (key, rest) = ex
                    if rest:
                        # leftover arguments
                        raise SyntaxError()

                    try:
                        declared_at = self.positions[id(ex)]
                    except KeyError:
                        declared_at = None

                    kid = self.child_context(args=[key], name="<catch>",
                                             declared_at=declared_at)

                    with kid as subc:
                        subc.helper_begin(act)
                        subc.pseudop_return()
                        code = subc.complete()

                    self.pseudop_dup()
                    self.add_expression(match)
                    self.pseudop_exception_match()
                    self.pseudop_pop_jump_if_false(label_next)
                    self.pseudop_pop()
                    self.pseudop_rot_two()
                    self.pseudop_pop()
                    self.pseudop_lambda(code)
                    self.pseudop_rot_two()
                    self.pseudop_call(1)

                else:
                    # an exception match without a binding

                    self.pseudop_dup()
                    self.add_expression(ex)
                    self.pseudop_exception_match()
                    self.pseudop_pop_jump_if_false(label_next)
                    self.pseudop_pop()
                    self.pseudop_pop()
                    self.pseudop_pop()
                    self.helper_begin(act)

                # we've handled the exception, there's nothing left on
                # the stack at this point but the result of the
                # handler.
                self.pseudop_return()

                if (3, 6) <= version_info:
                    pass
                elif (3, 3) <= version_info < (3, 6):
                    self.pseudop_pop_except()
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
            # first we have to cleanup, popping the finally block
            self.pseudop_pop_block()
            self.pseudop_const(None)
            self.pseudop_faux_pop(1)

            # here's the actual handling of the finally event
            self.pseudop_label(label_finally)
            self.helper_begin(act_finally)
            self.pseudop_return()

            if (3, 6) <= version_info:
                pass
            elif (3, 3) <= version_info < (3, 6):
                self.pseudop_end_finally()

        else:
            # guess we're all done, actually.
            self.pseudop_return_none()

        # no further transformations needed on the passed source code.
        return None


    @special(symbol("set-var"))
    def special_set_var(self, source):

        called_by, (binding, body) = source

        self.helper_begin(body)

        if is_symbol(binding):
            self.pseudop_set_var(str(binding))

        elif is_pair(binding):
            # TODO: implement
            assert(False)

        else:
            assert(False)

        # make set-var calls evaluate to None
        self.pseudop_const(None)

        # no additional transform needed
        return None


    @special(symbol("define"))
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


    @special(symbol("defun"))
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
            raise SyntaxError("formals must be symbol or pair, not %r" %
                              args)

        try:
            declared_at = self.positions[id(source)]
        except KeyError:
            declared_at = None

        kid = self.child_context(args=args, varargs=varargs,
                                 name=name,
                                 declared_at=declared_at)

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


    @special(symbol("defmacro"))
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
            raise SyntaxError("formals must be symbol or pair, not %r" %
                              args)

        try:
            declared_at = self.positions[id(source)]
        except KeyError:
            declared_at = None

        kid = self.child_context(args=args, varargs=varargs,
                                 name=name,
                                 declared_at=declared_at)

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


    @special(symbol("cond"))
    def special_cond(self, source):

        called_by, cl = source

        self.pseudop_label(self.gen_label())

        done = self.gen_label()
        label = None

        for test, body in cl.unpack():
            if label:
                self.pseudop_label(label)

            label = self.gen_label()

            if test is symbol("else"):
                self.helper_begin(body)
                self.pseudop_jump(done)

            else:
                self.add_expression(test)
                self.pseudop_pop_jump_if_false(label)
                self.helper_begin(body)
                self.pseudop_jump(done)

        self.pseudop_const(None)

        if label:
            self.pseudop_label(label)
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
    Calculates the maximum stack size from the pseudo operations
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
        # if stac < 0:
        # print("SHIT BROKE")
        # print(pseudops)
        assert(stac >= 0)

    # print("max_stack()")
    for op, *args in pseudops:
        # print(op, args, stac, maxc)

        if op is Pseudop.POSITION:
            pass

        elif op is Pseudop.CALL:
            pop(args[0])

        elif op is Pseudop.CONST:
            push()

        elif op is Pseudop.GET_VAR:
            push()

        elif op is Pseudop.SET_VAR:
            pop()

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
            assert(False)

    # if stac != 0:
    #     print("BOOM!", stac)
    #     print(pseudops)

    assert(stac == 0)
    return maxc


def apply_jump_labels(coll, jabs, jrel, labels):

    if (3, 6) <= version_info:
        for coll_offset, name in jabs:
            target = labels[name]

            coll_ext = coll[coll_offset]
            coll_jmp = coll[coll_offset + 1]

            coll_ext[1] = (target >> 8) & 0xff
            coll_jmp[1] = target & 0xff

        for coll_offset, name, off in jrel:
            target = labels[name]
            target -= (off + 4)

            coll_ext = coll[coll_offset]
            coll_jmp = coll[coll_offset + 1]

            coll_ext[1] = (target >> 8) & 0xff
            coll_jmp[1] = target & 0xff

    elif (3, 3) <= version_info < (3, 6):
        for coll_offset, name in jabs:
            target = labels[name]

            coll_jmp = coll[coll_offset]

            coll_jmp[1] = target & 0xff
            coll_jmp[2] = (target >> 8) & 0xff

        for coll_offset, name, off in jrel:
            target = labels[name]
            target -= (off + 3)

            coll_jmp = coll[coll_offset]

            coll_jmp[1] = target & 0xff
            coll_jmp[2] = (target >> 8) & 0xff

    else:
        raise UnsupportedVersion(version_info)


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

    for sym, meth in SpecialsCodeSpace.all_specials():
        yield str(sym), Special(gen_wrapper(meth), name=sym)


def compile_from_ast(astree, env, filename=None):
    positions = {}

    cl = astree.simplify(positions)

    codespace = SpecialsCodeSpace(filename=filename,
                                  positions=positions)

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
