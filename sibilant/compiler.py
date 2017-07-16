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

from . import car, cdr, constype, nil, symbol
from .ast import compose_from_str, compose_from_stream


__all__ = (
    "Opcode", "Pseudop",
    "CodeSpace", "compile_from_str", "compile_from_stream",
    "compile_from_ast",
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
        return  self.value in dis.hasconst

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


# specials defined internal to the compiler have these symbols

_define_sym = symbol("define")
_defmacro_sym = symbol("defmacro")
_defun_sym = symbol("defun")
_else_sym = symbol("else")
_if_sym = symbol("if")
_lambda_sym = symbol("lambda")
_let_sym = symbol("let")
_progn_sym = symbol("progn")
_quasiquote_sym = symbol("quasiquote")
_quote_sym = symbol("quote")
_setf_sym = symbol("set!")
_splice_sym = symbol("splice")
_unquote_sym = symbol("unquote")


class CodeSpace(object):
    """
    Represents a lexical scope, expressions occurring within that
    scope, and nested sub-scopes.
    """

    def __init__(self, env, parent=None):
        self.env = env
        self.parent = parent

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


    def child(self):
        """
        Create a chile CodeSpace
        """
        return CodeSpace(self.env, parent=self)


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
        if ((name in self.fast_vars) or
            (name in self.free_vars) or
            (name in self.cell_vars) or
            (name in self.global_vars)):

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


    def find_special(self, namesym):
        if namesym is _quote_sym:
            return self.special_quote

        elif namesym is _quasiquote_sym:
            return self.special_quasiquote

        elif namesym is _let_sym:
            return self.special_let

        elif namesym is _lambda_sym:
            return self.special_lambda

        elif namesym is _progn_sym:
            return self.special_progn

        elif namesym is _define_sym:
            return self.special_define

        elif namesym is _defun_sym:
            return self.special_defun

        elif namesym is _defmacro_sym:
            return self.special_defmacro

        elif namesym is _setf_sym:
            return self.special_setf

        else:
            macro = find_macro(str(namesym), self.env)
            if macro:
                return macro.__special__

        return None


    def special_quote(self, body):
        """
        Special form for quote
        """

        if body is nil:
            self.pseudop_get_var("nil")

        elif isinstance(body, symbol):
            self.pseudop_get_var("symbol")
            self.pseudop_const(str(body))
            self.pseudop(Pseudop.APPLY, 1)

        elif isinstance(body, constype):
            self.pseudop_get_var("cons")
            cl = body.count()
            for c in body.unpack():
                self.special_quote(c)
            if body.is_proper():
                cl += 1
                self.pseudop_get_var("nil")
            self.pseudop(Pseudop.APPLY, cl)

        else:
            self.pseudop_const(body)

        # no additional transform needed
        return None


    def special_quasiquote(self, body):
        """
        Special form for quasiquote
        """
        return None


    def special_apply(self, c):
        for pos in c.unpack():
            self.add_expression(pos)

        self.pseudop(Pseudop.APPLY, c.count() - 1)

        # no additional transform needed
        return None


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
                self.pseudop(Pseudop.POP)
            self.add_expression(c)

        # no additional transform needed
        return None


    def special_lambda(self, cl):
        """
        Special form for lambda
        """

        subc = self.child()
        args, body = cl
        for arg in args.unpack():
            subc.declare_arg(arg)
        subc.special_progn(body)
        subc.pseudop_return()

        code = subc.complete()
        self.pseudop_lambda(code)

        # no additional transform needed
        return None


    def special_let(self, cl):

        subc = self.child()
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

        self.pseudop(Pseudop.APPLY, len(vals))

        # no additional transform needed
        return None


    def special_setf(self, cl):

        binding, body = cl

        self.special_progn(body)
        self.pseudop(Pseudop.DUP)

        if isinstance(binding, symbol):
            self.pseudop_set_var(str(binding))

        elif isinstance(binding, constype):
            pass

        else:
            assert(False)


    def special_define(self, cl):
        binding, body = cl

        self.special_progn(body)
        self.pseudop(Pseudop.DUP)

        if isinstance(binding, symbol):
            self.pseudop_define(str(binding))
        else:
            assert(False)


    def pseudop(self, *op_args):
        """
        Pushes a pseudo op and arguments into the code
        """
        self.pseudops.append(op_args)


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


    def add_expression(self, expr):
        while True:
            if expr is nil:
                return

            elif isinstance(expr, constype):
                head, tail = expr

                if isinstance(head, symbol):
                    # see if this is a special, either a builtin one
                    # or a defined macro.
                    special = self.find_special(head)
                    if special:
                        expr = special(tail)
                        if expr is None:
                            return
                        else:
                            continue

                # either not a symbol, or it was and the symbol wasn't
                # a special.
                return self.special_apply(expr)

            elif isinstance(expr, symbol):
                return self.pseudop_get_var(str(expr))

            else:
                # TODO there are some literal types that can't be used
                # as constants, will need to fill those in here
                return self.pseudop_const(expr)


    def add_expression_with_pop(self, expr):
        self.add_expression(expr)
        self.pseudop(Pseudop.POP)


    def add_expression_with_return(self, expr):
        self.add_expression(expr)
        self.pseudop(Pseudop.RET_VAL)


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

        code = b''.join([(bytes([o.value, *args])) for o, *args
                         in self.gen_code()])

        consts = tuple(self.consts)
        names = tuple(self.names)
        varnames = *self.fast_vars, *self.cell_vars
        filename = "<sibilant>"
        name = "<lambda>"

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
                yield Opcode.MAKE_FUNCTION, 0x08, 0
            elif (3, 3) <= version_info < (3, 6):
                yield Opcode.MAKE_CLOSURE, 0x00, 0
            else:
                #print("Unsupported:", _PYVER)
                assert(False)

        else:
            # not a closure, so just a pain ol' function
            yield Opcode.LOAD_CONST, ci, 0
            yield Opcode.LOAD_CONST, ni, 0
            yield Opcode.MAKE_FUNCTION, 0x00, 0


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
        # if stac < 0:
        #     print("SHIT BROKE")
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

    def __special__(self, cl):
        return self.__expand__(*cl.unpack())


def find_macro(name, env):
    val = None

    if name in env:
        val = env[name]

    elif "__builtins__" in env:
        env = env["__builtins__"].__dict__
        if name in env:
            val = env[name]

    if val and isinstance(val, Macro):
        return val
    else:
        return None


def compile_from_ast(astree, env):
    positions = {}
    codespace = CodeSpace(env)
    codespace.add_expression_with_return(astree.simplify(positions))
    return codespace.complete()


def compile_from_stream(stream, env):
    astree = compose_from_stream(stream)
    return compile_from_ast(astree, env)


def compile_from_str(src_str, env):
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
