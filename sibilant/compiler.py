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
from functools import wraps
from sys import version_info
from types import CodeType

from . import is_cons, nil, symbol
from .ast import (
    compose_from_str, compose_from_stream,
    compose_all_from_str, compose_all_from_stream,
)


__all__ = (
    "Opcode", "Pseudop", "CodeSpace",
    "Macro", "SpecialsCodeSpace",
    "compile_from_str", "compile_from_stream", "compile_from_ast",
)


def memoized():
    _unset = object()

    def memoized(fun):
        memory = _unset

        @wraps(fun)
        def wrapper(*args, **kwds):
            nonlocal memory
            if memory is _unset:
                memory = fun(*args, **kwds)
            return memory

        return wrapper

    return memoized


memoized = memoized()


class Opcode(Enum):

    @memoized
    def hasconst(self):
        return self.value in dis.hasconst

    @memoized
    def hasfree(self):
        return self.value in dis.hasfree

    @memoized
    def hasjabs(self):
        return self.value in dis.hasjabs

    @memoized
    def hasjrel(self):
        return self.value in dis.hasjrel

    @memoized
    def haslocal(self):
        return self.value in dis.haslocal

    @memoized
    def hasname(self):
        return self.value in dis.hasname

    @memoized
    def hasnargs(self):
        return self.value in dis.hasnargs


Opcode = Opcode("Opcode", dis.opmap)


class Pseudop(Enum):
    APPLY = 1
    CONST = 2
    GET_VAR = 3
    SET_VAR = 4
    POP = 5
    LAMBDA = 6
    RET_VAL = 7
    DEFINE = 8
    DUP = 9


def _list_unique_append(l, v):
    if v in l:
        return l.index(v)
    else:
        l.append(v)
        return len(l)


class CodeSpace(object):
    """
    Represents a lexical scope, expressions occurring within that
    scope, and nested sub-scopes.
    """

    def __init__(self, env, parent=None, name=None, filename=None):
        self.env = env
        self.parent = parent
        self.name = name
        self.filename = filename

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
        self.names = []

        # first const is required -- it'll be None or a doc string
        self.consts = [None]

        self.pseudops = []


    def child(self, name=None):
        """
        Create a chile CodeSpace
        """
        return type(self)(self.env, parent=self, name=name,
                          filename=self.filename)


    def declare_const(self, value):
        return _list_unique_append(self.consts, value)


    def declare_arg(self, name):
        name = str(name)
        _list_unique_append(self.args, name)
        _list_unique_append(self.fast_vars, name)


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


    def pseudop(self, *op_args):
        """
        Pushes a pseudo op and arguments into the code
        """
        self.pseudops.append(op_args)


    def pseudop_apply(self, argc):
        self.pseudop(Pseudop.APPLY, argc)


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

        flags = 0x12  # NEWLOCALS, NESTED

        if (3, 6) <= version_info:
            # filter so that all opcodes have exactly one argument
            gen_code = (((opa[0], 0) if len(opa) < 2 else opa[:2])
                        for opa in self.gen_code())
            code = b''.join([(bytes([o.value, a])) for o, a
                             in gen_code])

        elif (3, 3) <= version_info < (3, 6):
            # emitted code has the correct number of arguments
            code = b''.join([(bytes([o.value, *args])) for o, *args
                             in self.gen_code()])

        else:
            # print("Unsupported:", _PYVER)
            assert(False)

        consts = tuple(self.consts)
        names = tuple(self.names)
        varnames = *self.fast_vars, *self.cell_vars
        filename = "<sibilant>" if self.filename is None else self.filename
        name = "<anon>" if self.name is None else self.name

        # TODO: create a line number table
        firstlineno = 1
        lnotab = b""

        freevars = tuple(self.free_vars)
        cellvars = tuple(self.cell_vars)

        ret = CodeType(argcount, 0, nlocals, stacksize, flags, code,
                       consts, names, varnames, filename, name,
                       firstlineno, lnotab, freevars, cellvars)

        # print("completed a CodeSpace", ret)
        # dis.show_code(ret)
        # print("Disassembly:")
        # dis.dis(ret)
        # print()

        return ret


    def gen_code(self):
        """
        Given the pseudo operations added to this CodeSpace, the named
        variables declared and requested, yield the CPython opcodes
        and arguments to represent this code.
        """

        # print("gen_code()")
        # for op, *args in self.pseudops:
        #     print(op, args)

        for op, *args in self.pseudops:
            if op is Pseudop.APPLY:
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
                # a special.
                return self.special_apply(expr)

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
            self.pseudop_apply(1)

        elif is_pair(body):
            self.pseudop_get_var("cons")
            cl = body.count()
            for c in body.unpack():
                self.special_quote(c)
            if body.is_proper():
                cl += 1
                self.pseudop_get_var("nil")
            self.pseudop_apply(cl)

        else:
            self.pseudop_const(body)

        # no additional transform needed
        return None


    @special(symbol("quasiquote"))
    def special_quasiquote(self, body):
        """
        Special form for quasiquote
        """
        return None


    @special(symbol("apply"))
    def special_apply(self, c):
        for pos in c.unpack():
            self.add_expression(pos)

        self.pseudop_apply(c.count() - 1)

        # no additional transform needed
        return None


    @special(symbol("progn"))
    def special_progn(self, cl):
        """
        Special form for progn
        """

        if not cl:
            # because all things are expressions, an empty progn still
            # needs to have a return value. In this case, the return value
            # will by the python None
            return self.pseudop_const(None)

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

        subc = self.child(name="<lambda>")
        args, body = cl
        for arg in args.unpack():
            subc.declare_arg(arg)
        subc.special_progn(body)
        subc.pseudop_return()

        code = subc.complete()
        self.pseudop_lambda(code)

        # no additional transform needed
        return None


    @special(symbol("let"))
    def special_let(self, cl):

        subc = self.child(name="<let>")
        bindings, body = cl

        vals = []
        for arg in bindings.unpack():
            name, val = arg.unpack()
            subc.declare_arg(name)
            vals.append(val)
        subc.special_progn(body)
        subc.pseudop_return()

        code = subc.complete()
        self.pseudop_lambda(code)

        for val in vals:
            self.add_expression(val)

        self.pseudop_apply(len(vals))

        # no additional transform needed
        return None


    @special(symbol("set!"))
    def special_setf(self, cl):

        binding, body = cl

        self.special_progn(body)
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

        self.special_progn(body)

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

        subc = self.child(name=name)
        args, body = cl
        for arg in args.unpack():
            subc.declare_arg(arg)
        subc.special_progn(body)
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

        subc = self.child(name=name)
        args, body = cl
        for arg in args.unpack():
            subc.declare_arg(arg)
        subc.special_progn(body)
        subc.pseudop_return()

        code = subc.complete()

        self.pseudop_get_var("macro")
        self.pseudop_lambda(code)
        self.pseudop_apply(1)

        self.pseudop_define(name)

        # defmacro expression evaluates to None
        self.pseudop_const(None)

        # no additional transform needed
        return None


def max_stack(pseudops):
    """
    Calculates the maximum stack size from the pseudo operations
    """

    maxc = 0
    stac = 0

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

        if op is Pseudop.APPLY:
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

        else:
            assert(False)

    assert(stac == 0)
    return maxc


class Macro(object):
    def __init__(self, fun):
        self.__expand__ = fun
        self.__name__ = fun.__name__

    def __special__(self, cl):
        return self.__expand__(*cl.unpack())

    def __call__(self, *args, **kwds):
        raise TypeError("attempt to call macro as function", self.__name__)


def is_macro(value):
    return isinstance(value, Macro)


def compile_from_ast(astree, env, filename=None):
    positions = {}
    codespace = SpecialsCodeSpace(env, filename=filename)
    codespace.add_expression_with_return(astree.simplify(positions))
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
