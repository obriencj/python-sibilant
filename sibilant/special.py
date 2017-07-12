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
from types import CodeType

from . import constype, symbol


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
    RETURN = 7


def _list_unique_append(l, v):
    if v in l:
        return l.index(v)
    else:
        l.append(v)
        return len(l)


class CodeSpace(object):

    def __init__(self, parent=None):
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
        self.consts = []

        self.pseudops = []


    def child(self):
        return CodeSpace(self)


    def is_closure(self):
        return bool(self.free_vars)


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

            if self.parent and self.parent._request_cell(name):
                # we asked our parent if we can get it as a closure,
                # and they said yes
                _list_unique_append(self.free_vars, name)
            else:
                _list_unique_append(self.global_vars, name)
                _list_unique_append(self.names, name)


    def _request_cell(self, name):
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

        elif self.parent and self.parent._request_cell(name):
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


    def max_stack(self):
        return max_stack(self.pseudops)


    def gen_code(self):
        for op, *args in self.pseudops:
            if op is Pseudop.APPLY:
                yield Opcode.CALL_FUNCTION, args[0] - 1, 0

            elif op is Pseudop.CONST:
                i = self.consts.index(args[0])
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
                pass

            elif op is Pseudop.POP:
                yield Opcode.POP_TOP,

            elif op is Pseudop.LAMBDA:
                c = args[0]
                i = self.consts.index(c)
                if c.co_freevars:
                    # closure
                    for n in c.co_freevars:
                        if n in self.cell_vars:
                            i = self.cell_vars.index(n)
                        elif n in self.free_vars:
                            i = self.free_vars.index(n) + len(self.cell_vars)
                        else:
                            assert(False)
                        yield OpCode.LOAD_CLOSURE, i
                    yield OpCode.MAKE_TUPLE, len(c.co_freevars)
                    yield OpCode.LOAD_CONST, i
                    yield OpCode.MAKE_CLOSURE, 0

                else:
                    # function
                    yield OpCode.LOAD_CONST, i
                    yield Opcode.MAKE_FUNCTION, 0

            elif op is Pseudop.RETURN:
                yield Opcode.RETURN_VALUE,


    def pseudop(self, *op_args):
        self.pseudops.append(op_args)


    def pseudop_const(self, val):
        self.declare_const(val)
        self.pseudop(Pseudop.CONST, val)


    def pseudop_var(self, name):
        self.request_var(name)
        self.pseudop(Pseudop.GET_VAR, name)


    def pseudop_expr(self, c):
        if isinstance(c, constype):
            self.pseudop_apply(c)
        elif isinstance(c, symbol):
            self.pseudop_var(str(c))
        else:
            self.pseudop_const(c)


    def pseudop_apply(self, c):
        for pos in c.unpack():
            self.pseudop_expr(pos)
        self.pseudop(Pseudop.APPLY, c.count())


    def pseudop_progn(self, cl):
        if not cl:
            # because all things are expressions, an empty progn still
            # needs to have a return value. In this case, the return value
            # will by the python None
            return self.pseudop_const(None)

        # interleave pops with expr, except for the last one
        first = True
        for c in cl:
            if first:
                first = False
            else:
                self.pseudop(Pseudop.POP)
            self.pseudop_expr(c)


    def pseudop_lambda(self, cl):

        subc = self.child()
        args, body = split_lambda(cl)
        for arg in args:
            subc.declare_arg(arg)
        subc.pseudop_progn(body)
        subc.pseudop(Pseudop.RETURN)

        code = subc.complete()
        self.pseudop_const(code)
        self.pseudop(Pseudop.LAMBDA, code)


    def pseudop_return(self):
        self.pseudop(Pseudop.RETURN)


    def pseudop_return_none(self):
        self.pseudop_const(None)
        self.pseudop(Pseudop.RETURN)


    def complete(self):
        argcount = len(self.args)

        # this is the number of fast variables, plus the variables
        # converted to cells for child scope usage
        nlocals = len(self.fast_vars) + len(self.cell_vars)

        stacksize = self.max_stack()

        flags = 0x12  # NEWLOCALS, NESTED

        code = b''.join([(bytes([o.value, *args])) for o, *args
                         in self.gen_code()])

        consts = tuple(self.consts)
        names = tuple(self.names)
        varnames = *self.args, *self.fast_vars
        filename = ""
        name = "<sibilant.lambda>"

        # TODO: create a line number table
        firstlineno = 0
        lnotab = b""

        freevars = tuple(self.free_vars)
        cellvars = tuple(self.cell_vars)

        return CodeType(argcount, 0, nlocals, stacksize, flags, code,
                        consts, names, varnames, filename, name,
                        firstlineno, lnotab, freevars, cellvars)


def max_stack(pseudops):
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
        assert(stac >= 0)

    for op, *args in pseudops:
        if op is Pseudop.APPLY:
            pop()
            pop(args[0])

        elif op is Pseudop.CONST:
            push()

        elif op is Pseudop.GET_VAR:
            push()

        elif op is Pseudop.SET_VAR:
            pop()

        elif op is Pseudop.POP:
            pop()

        elif op is Pseudop.LAMBDA:
            push()
            a = args[0].co_freevars
            if a:
                push(a)
                pop(a)
            pop()
            push()

        elif op is Pseudop.RETURN:
            pop()

        else:
            assert(False)

        assert(stac <= 1)
        return maxc

#
# The end.
