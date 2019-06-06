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
sibilant.pseudops.cpython36

Compiler target for CPython 3.6 bytecode

author: Christopher O'Brien <obriencj@gmail.com>
license: LGPL v.3
"""


from sibilant.pseudops import PseudopsCompiler, Pseudop, Opcode, translator
from sibilant.pseudops.stack import StackCounter, stacker
from sibilant.lib import symbol


def direct(opcode, hardarg=None):
    if hardarg is None:
        def direct_translate(comp, pseudop, args):
            try:
                yield opcode, args[0]
            except IndexError:
                print(pseudop, opcode, args)
                raise

    else:
        def direct_translate(comp, pseudop, args):
            yield opcode, hardarg

    return direct_translate


_P = Pseudop
_O = Opcode


class PseudopsCPython36(PseudopsCompiler):
    """
    Pseudops compiler emitting bytecode compatible with CPython
    version 3.6
    """


    _translations_ = {
        _P.BREAK_LOOP: direct(_O.BREAK_LOOP, 0),
        _P.CONTINUE_LOOP: direct(_O.CONTINUE_LOOP),
        _P.ROT_TWO: direct(_O.ROT_TWO, 0),
        _P.ROT_THREE: direct(_O.ROT_THREE, 0),
        _P.RET_VAL: direct(_O.RETURN_VALUE, 0),
        _P.YIELD_VAL: direct(_O.YIELD_VALUE, 0),
        _P.YIELD_FROM: direct(_O.YIELD_FROM, 0),
        _P.DUP: direct(_O.DUP_TOP, 0),
        _P.POP: direct(_O.POP_TOP, 0),

        _P.COMPARE_OP: direct(_O.COMPARE_OP),

        _P.GET_ITEM: direct(_O.BINARY_SUBSCR, 0),
        _P.SET_ITEM: direct(_O.STORE_SUBSCR, 0),
        _P.DEL_ITEM: direct(_O.DELETE_SUBSCR, 0),

        _P.ITER: direct(_O.GET_ITER, 0),
        _P.FOR_ITER: direct(_O.FOR_ITER),
        _P.GET_YIELD_FROM_ITER: direct(_O.GET_YIELD_FROM_ITER, 0),

        _P.UNPACK_SEQUENCE: direct(_O.UNPACK_SEQUENCE),

        _P.CALL: direct(_O.CALL_FUNCTION),
        _P.CALL_KW: direct(_O.CALL_FUNCTION_KW),
        _P.CALL_VAR: direct(_O.CALL_FUNCTION_EX, 0x00),
        _P.CALL_VAR_KW: direct(_O.CALL_FUNCTION_EX, 0x01),

        _P.BINARY_ADD: direct(_O.BINARY_ADD, 0),
        _P.BINARY_SUBTRACT: direct(_O.BINARY_SUBTRACT, 0),
        _P.BINARY_MULTIPLY: direct(_O.BINARY_MULTIPLY, 0),
        _P.BINARY_MATRIX_MULTIPLY: direct(_O.BINARY_MATRIX_MULTIPLY, 0),
        _P.BINARY_TRUE_DIVIDE: direct(_O.BINARY_TRUE_DIVIDE, 0),
        _P.BINARY_FLOOR_DIVIDE: direct(_O.BINARY_FLOOR_DIVIDE, 0),
        _P.BINARY_POWER: direct(_O.BINARY_POWER, 0),
        _P.BINARY_MODULO: direct(_O.BINARY_MODULO, 0),
        _P.BINARY_LSHIFT: direct(_O.BINARY_LSHIFT, 0),
        _P.BINARY_RSHIFT: direct(_O.BINARY_RSHIFT, 0),
        _P.BINARY_AND: direct(_O.BINARY_AND, 0),
        _P.BINARY_OR: direct(_O.BINARY_OR, 0),
        _P.BINARY_XOR: direct(_O.BINARY_XOR, 0),

        _P.UNARY_POSITIVE: direct(_O.UNARY_POSITIVE, 0),
        _P.UNARY_NEGATIVE: direct(_O.UNARY_NEGATIVE, 0),
        _P.UNARY_NOT: direct(_O.UNARY_NOT, 0),
        _P.UNARY_INVERT: direct(_O.UNARY_INVERT, 0),

        _P.BUILD_TUPLE: direct(_O.BUILD_TUPLE),
        _P.BUILD_TUPLE_UNPACK: direct(_O.BUILD_TUPLE_UNPACK),
        _P.BUILD_MAP: direct(_O.BUILD_MAP),
        _P.BUILD_MAP_UNPACK: direct(_O.BUILD_MAP_UNPACK),
        _P.BUILD_LIST: direct(_O.BUILD_LIST),
        _P.BUILD_SET: direct(_O.BUILD_SET),
        _P.BUILD_SLICE: direct(_O.BUILD_SLICE),
        _P.BUILD_STR: direct(_O.BUILD_STRING),
        _P.FORMAT: direct(_O.FORMAT_VALUE),
        _P.RAISE: direct(_O.RAISE_VARARGS),

        _P.JUMP: direct(_O.JUMP_ABSOLUTE),
        _P.JUMP_FORWARD: direct(_O.JUMP_FORWARD),
        _P.JUMP_IF_FALSE_OR_POP: direct(_O.JUMP_IF_FALSE_OR_POP),
        _P.JUMP_IF_TRUE_OR_POP: direct(_O.JUMP_IF_TRUE_OR_POP),
        _P.POP_JUMP_IF_FALSE: direct(_O.POP_JUMP_IF_FALSE),
        _P.POP_JUMP_IF_TRUE: direct(_O.POP_JUMP_IF_TRUE),
        _P.SETUP_WITH: direct(_O.SETUP_WITH),
        _P.WITH_CLEANUP_START: direct(_O.WITH_CLEANUP_START, 0),
        _P.WITH_CLEANUP_FINISH: direct(_O.WITH_CLEANUP_FINISH, 0),
        _P.SETUP_EXCEPT: direct(_O.SETUP_EXCEPT),
        _P.SETUP_FINALLY: direct(_O.SETUP_FINALLY),
        _P.SETUP_LOOP: direct(_O.SETUP_LOOP),
        _P.POP_BLOCK: direct(_O.POP_BLOCK, 0),
        _P.POP_EXCEPT: direct(_O.POP_EXCEPT, 0),
        _P.END_FINALLY: direct(_O.END_FINALLY, 0),

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
            op, arg = opa

            if op.hasjabs():
                # deal with jumps, so we can set their argument
                # to an appropriate label offset later

                mark_jabs(arg)
                # we are being lazy here, and padding out our
                # jumps with an EXTENDED_ARG, in case we need more
                # than 8 bits of address once labels are applied.
                coll.append([Opcode.EXTENDED_ARG, 0])
                coll.append([op, 0])
                offset += 4

            elif op.hasjrel():
                # relative jump!

                mark_jrel(arg)
                coll.append([Opcode.EXTENDED_ARG, 0])
                coll.append([op, 0])
                offset += 4

            else:
                if arg > 0xff:
                    coll.append([Opcode.EXTENDED_ARG, arg >> 8])
                    coll.append([op, arg & 0xff])
                    offset += 4
                else:
                    coll.append(opa)
                    offset += 2

        if jabs or jrel or labels:
            # Given our labels, modify jmp calls to point to the label
            self.apply_jump_labels(coll, jabs, jrel, labels)

        result = []
        for c, a in coll:
            result.append(c.value)
            result.append(a)

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
                # print("deferring on %r, %r" % (op, args))
                yield from self.translate(op, args)


    def apply_jump_labels(self, coll, jabs, jrel, labels):
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
                # in version 3.6 and beyond, negative line numbers
                # work fine, so a CALL_FUNCTION can correctly state
                # that it happens at line it started on, rather than
                # on the line it closes at
                pass

            if d_line < -128 or d_line > 127:
                dd_line = (d_line >> 8) & 0xff
                gathered.append(bytes([d_offset, dd_line]))

            d_line &= 0xff
            gathered.append(bytes([d_offset, d_line]))

            prev_offset = offset
            prev_line = line

        res = firstline, b''.join(gathered)
        return res


    def pseudop_call(self, argc):
        # changed in 3.6, CALL is used for invocations with only
        # positional arguments.
        self.pseudop(Pseudop.CALL, argc)


    def pseudop_lambda(self, code, defaults=(), kwonly=()):
        self.declare_const(code)
        self.declare_const(code.co_name)

        default_c = len(defaults)
        kwonly_c = len(kwonly)

        if default_c:
            for arg, expr in defaults:
                self.add_expression(expr)
            self.pseudop_build_tuple(default_c)

        if kwonly_c:
            for arg, expr in kwonly:
                self.pseudop_const(str(arg))
                self.add_expression(expr)
            self.pseudop_build_map(kwonly_c)

        return self.pseudop(Pseudop.LAMBDA, code, default_c, kwonly_c)


    def pseudop_build_str(self, count):
        return self.pseudop(Pseudop.BUILD_STR, count)


    def pseudop_format(self, flags):
        return self.pseudop(Pseudop.FORMAT, flags)


    def pseudop_get_method(self, namesym):
        return self.pseudop_get_attr(namesym)


    def pseudop_call_method(self, argc):
        return self.pseudop_call(argc)


    @translator(Pseudop.LAMBDA)
    def translate_lambda(self, pseudop, args):
        """
        Helper to _gen_code that handles just lambda definitions
        """

        # as per pseudop_lambda, the args will be a triplet
        code, default_count, kwonly_count = args

        ci = self.consts.index(code)
        ni = self.consts.index(code.co_name)

        _Opcode = Opcode

        flags = 0x00

        if default_count:
            flags |= 0x01

        if kwonly_count:
            flags |= 0x02

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
                yield _Opcode.LOAD_CLOSURE, fi

            yield _Opcode.BUILD_TUPLE, len(code.co_freevars)
            flags |= 0x08

        # not a closure, so just a pain ol' function
        yield _Opcode.LOAD_CONST, ci
        yield _Opcode.LOAD_CONST, ni
        yield _Opcode.MAKE_FUNCTION, flags


    @translator(Pseudop.UNPACK_EX)
    def translator_unpack_ex(self, pseudop, args):
        if args[1]:
            yield Opcode.EXTENDED_ARG, args[1]
        yield Opcode.UNPACK_EX, args[0]


    @translator(Pseudop.CONST)
    def translator_const(self, pseudop, args):
        i = _const_index(self.consts, args[0])
        yield Opcode.LOAD_CONST, i


    @translator(Pseudop.LOAD_CELL)
    def translator_load_cell(self, pseudop, args):
        n = args[0]
        if n in self.cell_vars:
            i = self.cell_vars.index(n)
            yield Opcode.LOAD_CLOSURE, i
        elif n in self.free_vars:
            i = self.free_vars.index(n) + len(self.cell_vars)
            yield Opcode.LOAD_CLOSURE, i
        else:
            assert False, "missing cell name %r" % n


    @translator(Pseudop.SET_LOCAL)
    def translator_set_local(self, pseudop, args):
        n = args[0]
        if n in self.cell_vars:
            i = self.cell_vars.index(n)
            yield Opcode.STORE_DEREF, i
        elif n in self.free_vars:
            i = self.free_vars.index(n) + len(self.cell_vars)
            yield Opcode.STORE_DEREF, i
        elif n in self.fast_vars:
            i = self.fast_vars.index(n)
            yield Opcode.STORE_FAST, i
        else:
            assert False, "missing local name %r" % n


    @translator(Pseudop.GET_VAR)
    def translator_get_var(self, pseudop, args):
        n = args[0]
        if n in self.cell_vars:
            i = self.cell_vars.index(n)
            yield Opcode.LOAD_DEREF, i
        elif n in self.free_vars:
            i = self.free_vars.index(n) + len(self.cell_vars)
            yield Opcode.LOAD_DEREF, i
        elif n in self.fast_vars:
            i = self.fast_vars.index(n)
            yield Opcode.LOAD_FAST, i
        elif n in self.global_vars:
            i = self.names.index(n)
            yield Opcode.LOAD_GLOBAL, i
        else:
            assert False, "missing var %r" % n


    @translator(Pseudop.SET_VAR)
    def translator_set_var(self, pseudop, args):
        n = args[0]
        if n in self.cell_vars:
            i = self.cell_vars.index(n)
            yield Opcode.STORE_DEREF, i
        elif n in self.free_vars:
            i = self.free_vars.index(n) + len(self.cell_vars)
            yield Opcode.STORE_DEREF, i
        elif n in self.fast_vars:
            i = self.fast_vars.index(n)
            yield Opcode.STORE_FAST, i
        elif n in self.global_vars:
            i = self.names.index(n)
            yield Opcode.STORE_GLOBAL, i
        else:
            assert False, "missing var %r" % n


    @translator(Pseudop.DEL_VAR)
    def translator_del_var(self, pseudop, args):
        n = args[0]
        if n in self.cell_vars:
            i = self.cell_vars.index(n)
            yield Opcode.DELETE_DEREF, i
        elif n in self.free_vars:
            i = self.free_vars.index(n) + len(self.cell_vars)
            yield Opcode.DELETE_DEREF, i
        elif n in self.fast_vars:
            i = self.fast_vars.index(n)
            yield Opcode.DELETE_FAST, i
        elif n in self.global_vars:
            i = self.names.index(n)
            yield Opcode.DELETE_GLOBAL, i
        else:
            assert False, "missing var %r" % n


    @translator(Pseudop.GET_GLOBAL)
    def translator_get_global(self, pseudop, args):
        n = args[0]
        if n in self.global_vars:
            i = self.names.index(n)
            yield Opcode.LOAD_GLOBAL, i
        else:
            assert False, "missing global name %r" % n


    @translator(Pseudop.SET_GLOBAL)
    def translator_set_global(self, pseudop, args):
        n = args[0]
        if n in self.global_vars:
            i = self.names.index(n)
            yield Opcode.STORE_GLOBAL, i
        else:
            assert False, "missing global name %r" % n


    @translator(Pseudop.DEL_GLOBAL)
    def translator_del_global(self, pseudop, args):
        n = args[0]
        if n in self.global_vars:
            i = self.names.index(n)
            yield Opcode.DELETE_GLOBAL, i
        else:
            assert False, "missing global name %r" % n


    @translator(Pseudop.GET_ATTR)
    def translator_get_attr(self, pseudop, args):
        n = args[0]
        i = self.names.index(n)
        yield Opcode.LOAD_ATTR, i


    @translator(Pseudop.SET_ATTR)
    def translator_set_attr(self, pseudop, args):
        n = args[0]
        i = self.names.index(n)
        yield Opcode.STORE_ATTR, i


    @translator(Pseudop.DEL_ATTR)
    def translator_del_attr(self, pseudop, args):
        n = args[0]
        i = self.names.index(n)
        yield Opcode.DELETE_ATTR, i


    @translator(Pseudop.IMPORT_NAME)
    def translator_import_name(self, pseudop, args):
        n = args[0]
        i = self.names.index(n)
        yield Opcode.IMPORT_NAME, i


    @translator(Pseudop.IMPORT_FROM)
    def translator_import_from(self, pseudop, args):
        n = args[0]
        i = self.names.index(n)
        yield Opcode.IMPORT_FROM, i


    def stack_counter(self, start_size=0):
        return StackCounterCPython36(self, start_size)


def _const_index(of_list, value):
    # have to use an `is` comparator, because the list.index method
    # will consider False and 0, and True and 1 to be equivalent,
    # breaking any constant pools containing those values.

    index = -1

    if value in [0, 1]:
        for index, found in enumerate(of_list):
            if found is value:
                return index
    else:
        try:
            index = of_list.index(value)
        except ValueError:
            pass
        else:
            return index

    if index == -1:
        print("missing const pool", list(map(repr, of_list)))
        assert False, "missing constant pool index for value %r" % value


class StackCounterCPython36(StackCounter):


    @stacker(Pseudop.CALL)
    def stacker_call(self, pseudop, args, push, pop):
        pop(args[0] + 1)  # positionals, function
        push()            # result


    @stacker(Pseudop.CALL_VAR)
    def stacker_call_var(self, pseudop, args, push, pop):
        pop(2)  # args, function
        push()  # result


    @stacker(Pseudop.CALL_VAR_KW)
    def stacker_call_var_kw(self, pseudop, args, push, pop):
        pop(3)  # kwargs, args, function
        push()  # result


    @stacker(Pseudop.LAMBDA)
    def stacker_lambda(self, pseudop, args, push, pop):
        if args[1]:
            pop()   # defaults tuple
        if args[2]:
            pop()   # kwonly defaults dict
        push()      # function inst


    @stacker(Pseudop.FORMAT)
    def stacker_format(self, pseudop, args, push, pop):
        pop()
        if args[0] & 0x04:
            pop()
        push()


    @stacker(Pseudop.BUILD_STR)
    def stacker_build_str(self, pseudop, args, push, pop):
        pop(args[0])
        push()


#
# The end.
