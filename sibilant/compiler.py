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

from enum import Enum
from sys import version_info
from types import CodeType

from . import (
    nil, symbol, is_pair, is_list, is_symbol,
    SibilantException, NotYetImplemented
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


class UnsupportedVersion(NotYetImplemented):
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


class Pseudop(Enum):
    POP = 1
    DUP = 2
    CALL = 3
    CALL_VARARGS = 4
    CONST = 5
    GET_VAR = 6
    SET_VAR = 7
    LAMBDA = 8
    RET_VAL = 9
    DEFINE = 10
    JUMP = 11
    POP_JUMP_IF_TRUE = 12
    POP_JUMP_IF_FALSE = 13
    BUILD_TUPLE = 14
    BUILD_TUPLE_UNPACK = 15
    POSITION = 100
    LABEL = 101


def _list_unique_append(l, v):
    if v in l:
        return l.index(v)
    else:
        l.append(v)
        return len(l)


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

    def __init__(self, env, args=(), kwargs=None, varargs=False,
                 parent=None, name=None,
                 filename=None, positions=None, declared_at=None):

        self.env = env
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


    def child(self, args=(), kwargs=None, varargs=False,
              name=None, declared_at=None):
        """
        Create a chile CodeSpace
        """

        if declared_at is None:
            declared_at = self.declared_at

        cs = type(self)(self.env, parent=self,
                        args=args, kwargs=kwargs,
                        varargs=varargs,
                        name=name,
                        filename=self.filename,
                        positions=self.positions,
                        declared_at=declared_at)

        return cs


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
        self.names.append(name)


    def _prep_varargs(self):
        # initial step which will convert the pythonic varargs tuple
        # into a proper cons list

        if self.declared_at:
            self.pseudop_position(*self.declared_at)

        self.pseudop_get_var("make-list")
        self.pseudop_get_var(self.args[-1])
        self.pseudop_call_varargs(0)
        self.pseudop_set_var(self.args[-1])


    def pseudop(self, *op_args):
        """
        Pushes a pseudo op and arguments into the code
        """
        self.pseudops.append(op_args)


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


    def pseudop_pop_jump_if_true(self, label_name):
        self.pseudop(Pseudop.POP_JUMP_IF_TRUE, label_name)


    def pseudop_pop_jump_if_false(self, label_name):
        self.pseudop(Pseudop.POP_JUMP_IF_FALSE, label_name)


    def pseudop_build_tuple(self, count):
        self.pseudop(Pseudop.BUILD_TUPLE, count)


    def pseudop_build_tuple_unpack(self, count):
        self.pseudop(Pseudop.BUILD_TUPLE_UNPACK, count)


    def complete(self):
        """
        Produces a python code object representing the state of this
        CodeSpace
        """

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

        jumps = []

        def mark_jump(label_name):
            jumps.append((len(coll), label_name))

        def set_position(line, col):
            lnt.append((offset, line, col))

        if (3, 6) <= version_info:
            for op, *args in self._gen_code(add_label, set_position):
                if op.hasjabs():
                    # deal with jumps, so we can set their argument
                    # to an appropriate label offset later

                    assert(args)
                    mark_jump(args[0])
                    # we are being lazy here, and padding out our
                    # jumps with an EXTENDED_ARG, in case we need more
                    # than 8 bits of address once labels are applied.
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
                    mark_jump(args[0])
                    coll.append([op, 0, 0])
                    offset += 3

                else:
                    coll.append((op, *args))
                    offset += (1 + len(args))

        else:
            raise UnsupportedVersion(version_info)

        if jumps or labels:
            # Given our labels, modify jmp calls to point to the label
            apply_jump_labels(coll, jumps, labels)

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

            elif op is Pseudop.JUMP:
                yield Opcode.JUMP_ABSOLUTE, args[0], 0

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

            else:
                assert(False)


    def _gen_lambda(self, code):
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
            _specials[namesym] = fun.__name__
            return fun
        return deco

    def find_special(self, namesym):
        try:
            # try and find a decorated special method first.
            return getattr(self, _specials[namesym], None)

        except KeyError:
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
                    found = None

            if found and is_macro(found):
                # we found a Macro instance, return the relevant
                # method
                return found.__special__
            else:
                # what we found doesn't qualify, throw it away
                return None

    return special, find_special


class SpecialsCodeSpace(CodeSpace):
    """
    Adds special forms to the basic functionality of CodeSpace
    """

    # decorator and lookup function for built-in special forms
    special, find_special = _special()


    def add_expression(self, expr):
        """
        Insert an expression into the code space. expr should be a cons
        cell representing the expression. If the expression appears to
        be a special form (either a macro defined in the CodeSpace's
        env, or a pre-defined built-in special), it will be expanded
        and compiled to pseudo ops.
        """

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
                        expr = special(tail)
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
                return self.pseudop_get_var(str(expr))

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


    @special(symbol("quote"))
    def special_quote(self, body):
        """
        Special form for quote
        """

        if body is nil:
            self.pseudop_get_var("nil")

        elif is_symbol(body):
            self.pseudop_get_var("symbol")
            self.pseudop_const(str(body))
            self.pseudop_call(1)

        elif is_pair(body):
            if is_list(body):
                self.pseudop_get_var("make-list")
            else:
                self.pseudop_get_var("cons")
            for cl, c in enumerate(body.unpack(), 1):
                self.special_quote(c)
            self.pseudop_call(cl)

        else:
            self.pseudop_const(body)

        # no additional transform needed
        return None


    @special(symbol("quasiquote"))
    def special_quasiquote(self, body):
        """
        Special form for quasiquote
        """

        # print(repr(body))

        if body is nil:
            self.pseudop_get_var("nil")

        elif is_symbol(body):
            self.pseudop_get_var("symbol")
            self.pseudop_const(str(body))
            self.pseudop_call(1)

        elif is_pair(body):
            if is_list(body):
                self.pseudop_get_var("make-list")
            else:
                self.pseudop_get_var("cons")

            coll_tup = 0
            curr_tup = 0

            for c in body.unpack():
                # print(" -- ", repr(c))

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
                        u_head, u_tail = tail
                        if u_head is symbol("splice"):
                            if coll_tup:
                                self.pseudop_build_tuple(coll_tup)
                                coll_tup = 0
                                curr_tup += 1
                            self.pseudop_get_var("py-tuple")
                            self.add_expression(u_tail)
                            self.pseudop_call(1)
                            curr_tup += 1
                        else:
                            self.add_expression(tail)
                            coll_tup += 1
                    else:
                        self.special_quasiquote(c)
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
    def special_begin(self, cl):
        """
        Special form for begin
        """

        if not cl:
            # because all things are expressions, an empty begin still
            # needs to have a return value. In this case, the return value
            # will by the python None
            return self.pseudop_const(None)

        self.pseudop_position_of(cl)

        # interleave pops with expr, except for the last one
        first = True
        for c in cl.unpack():
            if first:
                first = False
            else:
                self.pseudop_pop()
            self.add_expression(c)

        # no additional transform needed
        return None


    @special(symbol("lambda"))
    def special_lambda(self, cl):
        """
        Special form for lambda
        """

        args, body = cl

        if is_symbol(args):
            args = [str(args)]
            varargs = True

        elif is_pair(args):
            varargs = not is_list(args)
            args = map(str, args.unpack())

        else:
            raise SyntaxError("formals must be symbol or pair, not %r" %
                              args)

        try:
            declared_at = self.positions[id(cl)]
        except KeyError:
            declared_at = None

        subc = self.child(args=args, varargs=varargs,
                          name="<lambda>", declared_at=declared_at)

        subc.special_begin(body)
        subc.pseudop_return()

        code = subc.complete()
        self.pseudop_lambda(code)

        # no additional transform needed
        return None


    @special(symbol("let"))
    def special_let(self, cl):

        bindings, body = cl

        args = []
        vals = []
        for arg in bindings.unpack():
            name, val = arg.unpack()
            args.append(str(name))
            vals.append(val)

        try:
            declared_at = self.positions[id(cl)]
        except KeyError:
            declared_at = None

        subc = self.child(args=args, name="<let>",
                          declared_at=declared_at)

        subc.special_begin(body)
        subc.pseudop_return()

        code = subc.complete()
        self.pseudop_lambda(code)

        for val in vals:
            self.add_expression(val)

        self.pseudop_call(len(vals))

        # no additional transform needed
        return None


    @special(symbol("set!"))
    def special_setf(self, cl):

        binding, body = cl

        self.special_begin(body)
        self.pseudop_dup()

        if is_symbol(binding):
            self.pseudop_set_var(str(binding))

        elif is_pair(binding):
            # TODO: implement
            assert(False)

        else:
            assert(False)


    @special(symbol("define"))
    def special_define(self, cl):
        binding, body = cl

        self.special_begin(body)

        if is_symbol(binding):
            self.pseudop_define(str(binding))
        else:
            assert(False)

        # define expression evaluates to None
        self.pseudop_const(None)

        return None


    @special(symbol("defun"))
    def special_defun(self, cl):
        namesym, cl = cl
        name = str(namesym)

        args, body = cl

        if is_symbol(args):
            args = [str(args)]
            varargs = True

        elif is_pair(args):
            varargs = not is_list(args)
            args = map(str, args.unpack())

        else:
            raise SyntaxError("formals must be symbol or pair, not %r" %
                              args)

        try:
            declared_at = self.positions[id(cl)]
        except KeyError:
            declared_at = None

        subc = self.child(args=args, varargs=varargs, name=name,
                          declared_at=declared_at)

        subc.special_begin(body)
        subc.pseudop_return()

        code = subc.complete()
        self.pseudop_lambda(code)
        self.pseudop_define(name)

        # defun expression evaluates to None
        self.pseudop_const(None)

        # no additional transform needed
        return None


    @special(symbol("defmacro"))
    def special_defmacro(self, cl):
        namesym, cl = cl
        name = str(namesym)

        args, body = cl

        if is_symbol(args):
            args = [str(args)]
            varargs = True

        elif is_pair(args):
            varargs = not is_list(args)
            args = map(str, args.unpack())

        else:
            raise SyntaxError("formals must be symbol or pair, not %r" %
                              args)

        try:
            declared_at = self.positions[id(cl)]
        except KeyError:
            declared_at = None

        subc = self.child(args=args, varargs=varargs, name=name,
                          declared_at=declared_at)

        subc.special_begin(body)
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
    def special_cond(self, cl):

        self.pseudop_label(self.gen_label())

        done = self.gen_label()
        label = None

        for test, body in cl.unpack():
            if label:
                self.pseudop_label(label)

            label = self.gen_label()

            if test is symbol("else"):
                self.special_begin(body)
                self.pseudop_jump(done)

            else:
                self.add_expression(test)
                self.pseudop_pop_jump_if_false(label)
                self.special_begin(body)
                self.pseudop_jump(done)

        self.pseudop_const(None)

        if label:
            self.pseudop_label(label)
        self.pseudop_label(done)


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

        elif op is Pseudop.DUP:
            push()

        elif op is Pseudop.SET_VAR:
            pop()

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

        elif op is Pseudop.JUMP:
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

        else:
            assert(False)

    # if stac != 0:
    # print("BOOM!", stac)
    # print(pseudops)
    assert(stac == 0)
    return maxc


def apply_jump_labels(coll, jumps, labels):

    if (3, 6) <= version_info:
        for coll_offset, name in jumps:
            target = labels[name]

            coll_ext = coll[coll_offset]
            coll_jmp = coll[coll_offset + 1]

            coll_ext[1] = (target >> 8) & 0xff
            coll_jmp[1] = target & 0xff

    elif (3, 3) <= version_info < (3, 6):
        for coll_offset, name in jumps:
            target = labels[name]

            coll_jmp = coll[coll_offset]

            coll_jmp[1] = target & 0xff
            coll_jmp[2] = (target >> 8) & 0xff

    else:
        raise UnsupportedVersion(version_info)


def lnt_compile(lnt, firstline=None):
    if not lnt:
        return (1 if firstline is None else firstline), b''

    firstline = lnt[0][1] if firstline is None else firstline
    gathered = []

    prev_offset = 0
    prev_line = lnt[0][1]

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
            dd_line = (dd_line >> 8) & 0xff
            gathered.append(bytes([d_offset, dd_line]))

        d_line &= 0xff
        gathered.append(bytes([d_offset, d_line]))

        prev_offset = offset
        prev_line = line

    return firstline, b''.join(gathered)


def label_generator(formatstr="label_%04i"):
    counter = 0

    def gen_label():
        nonlocal counter
        counter += 1
        return formatstr % counter

    return gen_label


class Macro(object):
    def __init__(self, fun):
        self.__expand__ = fun
        self.__name__ = fun.__name__

    def __special__(self, cl):
        return self.__expand__(*cl.unpack())

    def __call__(self, *args, **kwds):
        raise TypeError("attempt to call macro as function", self.__name__)


def macro(fun):
    if is_macro(fun):
        return fun
    else:
        return Macro(fun)


def is_macro(value):
    return isinstance(value, Macro)


def compile_from_ast(astree, env, filename=None):
    positions = {}

    cl = astree.simplify(positions)

    codespace = SpecialsCodeSpace(env, filename=filename,
                                  positions=positions)

    codespace.add_expression_with_return(cl)

    return codespace.complete()


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
