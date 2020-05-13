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
sibilant.pseudops.cpython35

Compiler target for CPython 3.5 bytecode

author: Christopher O'Brien <obriencj@gmail.com>
license: LGPL v.3
"""


from sibilant.lib import symbol

from sibilant.pseudops import (
    PseudopsCompiler, Pseudop, Opcode,
    translator,
)

from sibilant.pseudops.stack import (
    StackCounter, stacker,
)


_symbol_format_value = symbol("__format_value__")
_symbol_build_string = symbol("__build_string__")


def direct(opcode, hasargs=True):
    if hasargs:
        def direct_translate(comp, pseudop, args):
            if len(args) == 1:
                yield opcode, args[0], 0
            else:
                yield (opcode, *args)

    else:
        def direct_translate(comp, pseudop, args):
            yield (opcode, )

    return direct_translate


_P = Pseudop
_O = Opcode


class PseudopsCPython35(PseudopsCompiler):
    """
    Pseudoops compiler emitting bytecode compatible with CPython
    version 3.5
    """


    _translations_ = {
        _P.BREAK_LOOP: direct(_O.BREAK_LOOP),
        _P.CONTINUE_LOOP: direct(_O.CONTINUE_LOOP),
        _P.ROT_TWO: direct(_O.ROT_TWO, False),
        _P.ROT_THREE: direct(_O.ROT_THREE, False),
        _P.DUP: direct(_O.DUP_TOP, False),
        _P.POP: direct(_O.POP_TOP, False),
        _P.RET_VAL: direct(_O.RETURN_VALUE, False),
        _P.YIELD_VAL: direct(_O.YIELD_VALUE, False),
        _P.YIELD_FROM: direct(_O.YIELD_FROM, False),
        _P.RAISE: direct(_O.RAISE_VARARGS),

        _P.COMPARE_OP: direct(_O.COMPARE_OP),

        _P.GET_ITEM: direct(_O.BINARY_SUBSCR),
        _P.SET_ITEM: direct(_O.STORE_SUBSCR),
        _P.DEL_ITEM: direct(_O.DELETE_SUBSCR),

        _P.ITER: direct(_O.GET_ITER),
        _P.FOR_ITER: direct(_O.FOR_ITER),
        _P.GET_YIELD_FROM_ITER: direct(_O.GET_YIELD_FROM_ITER),
        _P.GET_AWAITABLE: direct(_O.GET_AWAITABLE),
        _P.BINARY_ADD: direct(_O.BINARY_ADD),

        _P.BUILD_TUPLE: direct(_O.BUILD_TUPLE),
        _P.BUILD_TUPLE_UNPACK: direct(_O.BUILD_TUPLE_UNPACK),
        _P.BUILD_MAP: direct(_O.BUILD_MAP),
        _P.BUILD_MAP_UNPACK: direct(_O.BUILD_MAP_UNPACK),
        _P.BUILD_LIST: direct(_O.BUILD_LIST),
        _P.BUILD_SET: direct(_O.BUILD_SET),
        _P.BUILD_SLICE: direct(_O.BUILD_SLICE),
        _P.BUILD_MAP: direct(_O.BUILD_MAP),

        _P.CALL: direct(_O.CALL_FUNCTION),
        _P.CALL_KW: direct(_O.CALL_FUNCTION_KW),
        _P.CALL_VAR: direct(_O.CALL_FUNCTION_VAR),
        _P.CALL_VAR_KW: direct(_O.CALL_FUNCTION_VAR_KW),

        _P.UNPACK_SEQUENCE: direct(_O.UNPACK_SEQUENCE),
        _P.UNPACK_EX: direct(_O.UNPACK_EX),

        _P.JUMP: direct(_O.JUMP_ABSOLUTE),
        _P.JUMP_FORWARD: direct(_O.JUMP_FORWARD),
        _P.JUMP_IF_FALSE_OR_POP: direct(_O.JUMP_IF_FALSE_OR_POP),
        _P.JUMP_IF_TRUE_OR_POP: direct(_O.JUMP_IF_TRUE_OR_POP),
        _P.POP_JUMP_IF_FALSE: direct(_O.POP_JUMP_IF_FALSE),
        _P.POP_JUMP_IF_TRUE: direct(_O.POP_JUMP_IF_TRUE),
        _P.SETUP_WITH: direct(_O.SETUP_WITH),
        _P.WITH_CLEANUP_START: direct(_O.WITH_CLEANUP_START, False),
        _P.WITH_CLEANUP_FINISH: direct(_O.WITH_CLEANUP_FINISH, False),
        _P.SETUP_EXCEPT: direct(_O.SETUP_EXCEPT),
        _P.SETUP_FINALLY: direct(_O.SETUP_FINALLY),
        _P.SETUP_LOOP: direct(_O.SETUP_LOOP),
        _P.POP_BLOCK: direct(_O.POP_BLOCK, False),
        _P.POP_EXCEPT: direct(_O.POP_EXCEPT, False),
        _P.END_FINALLY: direct(_O.END_FINALLY, False),

        _P.UNARY_POSITIVE: direct(_O.UNARY_POSITIVE, False),
        _P.UNARY_NEGATIVE: direct(_O.UNARY_NEGATIVE, False),
        _P.UNARY_NOT: direct(_O.UNARY_NOT, False),
        _P.UNARY_INVERT: direct(_O.UNARY_INVERT, False),

        _P.BINARY_ADD: direct(_O.BINARY_ADD, False),
        _P.BINARY_SUBTRACT: direct(_O.BINARY_SUBTRACT, False),
        _P.BINARY_MULTIPLY: direct(_O.BINARY_MULTIPLY, False),
        _P.BINARY_MATRIX_MULTIPLY: direct(_O.BINARY_MATRIX_MULTIPLY, False),
        _P.BINARY_TRUE_DIVIDE: direct(_O.BINARY_TRUE_DIVIDE, False),
        _P.BINARY_FLOOR_DIVIDE: direct(_O.BINARY_FLOOR_DIVIDE, False),
        _P.BINARY_POWER: direct(_O.BINARY_POWER, False),
        _P.BINARY_MODULO: direct(_O.BINARY_MODULO, False),
        _P.BINARY_LSHIFT: direct(_O.BINARY_LSHIFT, False),
        _P.BINARY_RSHIFT: direct(_O.BINARY_RSHIFT, False),
        _P.BINARY_AND: direct(_O.BINARY_AND, False),
        _P.BINARY_XOR: direct(_O.BINARY_XOR, False),
        _P.BINARY_OR: direct(_O.BINARY_OR, False),
    }


    def code_bytes(self, lnt):
        offset = 0
        coll = []

        labels = {0: 0}

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

        for opa in self.gen_opcode(add_label, set_position):
            op, *args = opa

            if op.hasjabs():
                # deal with jumps, so we can set their argument to
                # an appropriate label offset later

                assert args, "hasjabs without target label"
                mark_jabs(args[0])
                coll.append([op, 0, 0])
                offset += 3

            elif op.hasjrel():
                # relative jump!

                assert args, "hasjrel without target label"
                mark_jrel(args[0])
                coll.append([op, 0, 0])
                offset += 3

            else:
                arglen = len(args)
                assert (arglen == 0) or (arglen == 2)
                coll.append((op, *args))
                offset += (1 + arglen)

        if jabs or jrel or labels:
            # Given our labels, modify jmp calls to point to the label
            self.apply_jump_labels(coll, jabs, jrel, labels)

        result = []
        for c, *a in coll:
            result.append(c.value)
            result.extend(a)

        return bytes(result)


    def gen_opcode(self, declare_label, declare_position):
        """
        Given the pseudo operations added to this CodeSpace, the named
        variables declared and requested, yield the CPython opcodes
        and arguments to represent this code.
        """

        _p_pos = Pseudop.POSITION
        _p_lab = Pseudop.LABEL

        for op, *args in self.gen_pseudops():
            if op is _p_pos:
                declare_position(*args)

            elif op is _p_lab:
                declare_label(args[0])

            else:
                yield from self.translate(op, args)


    def apply_jump_labels(self, coll, jabs, jrel, labels):
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


    def pseudop_build_str(self, count):
        # emulate the BUILD_STRING opcode using string joining

        self.pseudop_build_tuple(count)

        # self.pseudop_const("")
        # self.pseudop_get_attr(symbol("join"))

        self.pseudop_get_global(_symbol_build_string)

        self.pseudop_rot_two()
        self.pseudop_call(1)


    def pseudop_format(self, flags):
        # emulate the FORMAT_VALUE opcode using the format function

        # TOS format str if flags & 0x01
        # TOS2 value

        # self.pseudop_get_global(symbol("format"))
        self.pseudop_get_global(_symbol_format_value)

        if flags & 0x04:
            self.pseudop_rot_three()
            self.pseudop_call(2)
        else:
            self.pseudop_rot_two()
            self.pseudop_call(1)


    def pseudop_lambda(self, code, defaults=(), kwonly=()):
        self.declare_const(code)
        self.declare_const(code.co_name)

        default_c = len(defaults)
        kwonly_c = len(kwonly)

        for arg, expr in defaults:
            self.add_expression(expr)

        for arg, expr in kwonly:
            self.pseudop_const(str(arg))
            self.add_expression(expr)

        return self.pseudop(Pseudop.LAMBDA, code, default_c, kwonly_c)


    def pseudop_get_method(self, namesym):
        return self.pseudop_get_attr(namesym)


    def pseudop_call_method(self, argc):
        return self.pseudop_call(argc)


    @translator(Pseudop.LAMBDA)
    def translate_lambda(self, pseudop, args):
        """
        Helper to _gen_code that handles just lambda definitions
        """

        code, default_count, kwonly_count = args

        ci = self.consts.index(code)
        ni = self.consts.index(code.co_name)

        _Opcode = Opcode

        if code.co_freevars:
            # code is a closure, so we'll need to find the matching
            # free/cell vars and provide them.

            for f in code.co_freevars:
                fsym = symbol(f)
                if fsym in self.cell_vars:
                    fi = self.cell_vars.index(fsym)
                elif fsym in self.free_vars:
                    fi = len(self.cell_vars)
                    fi += self.free_vars.index(fsym)
                else:
                    assert False, "missing local var %r" % fsym

                yield _Opcode.LOAD_CLOSURE, fi, 0

            yield _Opcode.BUILD_TUPLE, len(code.co_freevars), 0
            yield _Opcode.LOAD_CONST, ci, 0
            yield _Opcode.LOAD_CONST, ni, 0
            yield _Opcode.MAKE_CLOSURE, default_count, kwonly_count

        else:
            # not a closure, so just a pain ol' function
            yield _Opcode.LOAD_CONST, ci, 0
            yield _Opcode.LOAD_CONST, ni, 0
            yield _Opcode.MAKE_FUNCTION, default_count, kwonly_count


    @translator(Pseudop.CONST)
    def translate_load_const(self, pseudop, args):
        i = _const_index(self.consts, args[0])
        yield Opcode.LOAD_CONST, i, 0


    @translator(Pseudop.LOAD_CELL)
    def translate_load_cell(self, pseudop, args):
        n = args[0]
        if n in self.cell_vars:
            i = self.cell_vars.index(n)
            yield Opcode.LOAD_CLOSURE, i, 0
        elif n in self.free_vars:
            i = self.free_vars.index(n) + len(self.cell_vars)
            yield Opcode.LOAD_CLOSURE, i, 0
        else:
            assert False, "missing cell name %r" % n


    @translator(Pseudop.SET_LOCAL)
    def translate_set_local(self, pseudop, args):
        n = args[0]
        if n in self.cell_vars:
            i = self.cell_vars.index(n)
            yield Opcode.STORE_DEREF, i, 0
        elif n in self.free_vars:
            i = self.free_vars.index(n) + len(self.cell_vars)
            yield Opcode.STORE_DEREF, i, 0
        elif n in self.fast_vars:
            i = self.fast_vars.index(n)
            yield Opcode.STORE_FAST, i, 0
        else:
            assert False, "missing local name %r" % n


    @translator(Pseudop.GET_VAR)
    def translate_get_var(self, pseudop, args):
        n = args[0]
        if n in self.cell_vars:
            i = self.cell_vars.index(n)
            yield Opcode.LOAD_DEREF, i, 0
        elif n in self.free_vars:
            i = self.free_vars.index(n) + len(self.cell_vars)
            yield Opcode.LOAD_DEREF, i, 0
        elif n in self.fast_vars:
            i = self.fast_vars.index(n)
            yield Opcode.LOAD_FAST, i, 0
        elif n in self.global_vars:
            i = self.names.index(n)
            yield Opcode.LOAD_GLOBAL, i, 0
        else:
            assert False, "missing var %r" % n


    @translator(Pseudop.SET_VAR)
    def translate_set_var(self, pseudop, args):
        n = args[0]
        if n in self.cell_vars:
            i = self.cell_vars.index(n)
            yield Opcode.STORE_DEREF, i, 0
        elif n in self.free_vars:
            i = self.free_vars.index(n) + len(self.cell_vars)
            yield Opcode.STORE_DEREF, i, 0
        elif n in self.fast_vars:
            i = self.fast_vars.index(n)
            yield Opcode.STORE_FAST, i, 0
        elif n in self.global_vars:
            i = self.names.index(n)
            yield Opcode.STORE_GLOBAL, i, 0
        else:
            assert False, "missing var %r" % n


    @translator(Pseudop.DEL_VAR)
    def translate_del_var(self, pseudop, args):
        n = args[0]
        if n in self.cell_vars:
            i = self.cell_vars.index(n)
            yield Opcode.DELETE_DEREF, i, 0
        elif n in self.free_vars:
            i = self.free_vars.index(n) + len(self.cell_vars)
            yield Opcode.DELETE_DEREF, i, 0
        elif n in self.fast_vars:
            i = self.fast_vars.index(n)
            yield Opcode.DELETE_FAST, i, 0
        elif n in self.global_vars:
            i = self.names.index(n)
            yield Opcode.DELETE_GLOBAL, i, 0
        else:
            assert False, "missing var %r" % n


    @translator(Pseudop.GET_GLOBAL)
    def translate_get_global(self, pseudop, args):
        n = args[0]
        if n in self.global_vars:
            i = self.names.index(n)
            yield Opcode.LOAD_GLOBAL, i, 0
        else:
            assert False, "missing global name %r" % n


    @translator(Pseudop.SET_GLOBAL)
    def translate_set_global(self, pseudop, args):
        n = args[0]
        if n in self.global_vars:
            i = self.names.index(n)
            yield Opcode.STORE_GLOBAL, i, 0
        else:
            assert False, "missing global name %r" % n


    @translator(Pseudop.DEL_GLOBAL)
    def translate_del_global(self, pseudop, args):
        n = args[0]
        if n in self.global_vars:
            i = self.names.index(n)
            yield Opcode.DELETE_GLOBAL, i, 0
        else:
            assert False, "missing global name %r" % n


    @translator(Pseudop.GET_ATTR)
    def translate_get_attr(self, pseudop, args):
        n = args[0]
        i = self.names.index(n)
        yield Opcode.LOAD_ATTR, i, 0


    @translator(Pseudop.SET_ATTR)
    def translate_set_attr(self, pseudop, args):
        n = args[0]
        i = self.names.index(n)
        yield Opcode.STORE_ATTR, i, 0


    @translator(Pseudop.DEL_ATTR)
    def translate_del_attr(self, pseudop, args):
        n = args[0]
        i = self.names.index(n)
        yield Opcode.DELETE_ATTR, i, 0


    @translator(Pseudop.IMPORT_NAME)
    def translate_import_name(self, pseudop, args):
        n = args[0]
        i = self.names.index(n)
        yield Opcode.IMPORT_NAME, i, 0


    @translator(Pseudop.IMPORT_FROM)
    def translate_import_from(self, pseudop, args):
        n = args[0]
        i = self.names.index(n)
        yield Opcode.IMPORT_FROM, i, 0


    def lnt_compile(self, lnt, firstline=None):
        if not lnt:
            return (1 if firstline is None else firstline), b''

        firstline = lnt[0][1] if firstline is None else firstline
        gathered = []

        prev_offset = 0
        prev_line = firstline

        for offset, line, _col in lnt:
            if gathered and line == prev_line:
                continue

            d_offset = (offset - prev_offset)
            d_line = (line - prev_line)

            d_offset &= 0xff

            if d_line < 0:
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
        return res


    def stack_counter(self, start_size=0):
        return StackCounterCPython35(self, start_size)


def _const_index(of_list, value):
    # have to use an `is` comparator, because the list.index method
    # will consider False and 0, and True and 1 to be equivalent,
    # breaking any constant pools containing those values.

    if value in [0, 1]:
        for index, found in enumerate(of_list):
            if found is value:
                return index
        else:
            assert False, "missing constant pool index for value %r" % value

    try:
        return of_list.index(value)
    except ValueError:
        assert False, "missing constant pool index for value %r" % value


class StackCounterCPython35(StackCounter):


    @stacker(Pseudop.CALL)
    def stacker_call(self, pseudop, args, push, pop):
        pop(args[0])      # positionals
        pop(args[1] * 2)  # kw:val pairs
        pop()             # function
        push()            # result


    @stacker(Pseudop.CALL_KW)
    def stacker_call_kw(self, pseudop, args, push, pop):
        pop(args[0])      # positionals
        pop(args[1] * 2)  # kw:val pairs
        pop(2)            # kwds, function
        push()            # result


    @stacker(Pseudop.CALL_VAR)
    def stacker_call_var(self, pseudop, args, push, pop):
        pop(args[0])      # positionals
        pop(args[1] * 2)  # kw:val pairs
        pop(2)            # args, function
        push()            # result


    @stacker(Pseudop.CALL_VAR_KW)
    def stacker_call_var_kw(self, pseudop, args, push, pop):
        pop(args[0])      # positionals
        pop(args[1] * 2)  # kw:val pairs
        pop(3)            # args, kwds, function
        push()            # result


    @stacker(Pseudop.LAMBDA)
    def stacker_lambda(self, pseudop, args, push, pop):
        pop(args[1])      # defaults
        pop(args[2] * 2)  # kwonly defaults
        push()            # function instance


#
# The end.
