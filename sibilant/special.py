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


    def add(*op_args):
        self.operations.append(op_args)


    def complete(self):
        pass



def special_apply(ast, code, env, scratch):
    """
    push the instructions for a function application into c
    """

    pass


def special_lambda(ast, code, env, scratch):
    """
    push the instructions for a lambda definition into c
    """

    pass


def special_syntax(ast, env, scratch):
    """
    Given a sibilant AST describing a syntax(macro), return an
    equivalent python AST
    """

    pass


def special_setter(ast, env, scratch):
    pass


def special_getter(ast, env, scratch):
    pass


def compile_apply(codespace, c):
    for pos in c:
        if is_list(c):
            compile_apply(codespace, c):
        elif is_const(c):
            codespace.add(LOAD_CONSTANT, c)
        elif is_symbol(c):
            codespace.add(LOAD_SYMBOL, c)
    codespace.add(CALL_FN, len(c))


def compile_progn(codespace, cl):
    first = True
    for c in cl:
        if first:
            first = False
        else:
            codespace.add(POP_TOP)

        if is_list(c):
            compile_apply(codespace, c)
        elif is_const(c):
            codespace.add(LOAD_CONSTANT, c)
        elif is_symbol(c):
            codespace.add(LOAD_SYMBOL, c)


def compile_lambda(codespace, cl):

    subc = codespace.child()
    args, body = split_lambda(cl)
    for arg in args:
        subc.declare_arg(arg)
    compile_progn(subc, body)
    subc.add(RETURN)

    codeobj = subc.to_code()
    codespace.add(LOAD_CONST, codeobj)
    codespace.add(CREATE_LAMBDA)


#
# The end.
