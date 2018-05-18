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
sibilant.compiler.cpython35

Compiler target for CPython 3.5 bytecode

author: Christopher O'Brien <obriencj@gmail.com>
license: LGPL v.3
"""


from . import (
    ExpressionCodeSpace, Pseudop, Opcode,
    gather_parameters,
)

from ..lib import symbol


class CPython35(ExpressionCodeSpace):
    """
    SpecialCodeSpace emitting bytecode compatible with CPython version
    3.5
    """

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
                coll.append((op, *args))
                offset += (1 + len(args))

        if jabs or jrel or labels:
            # Given our labels, modify jmp calls to point to the label
            apply_jump_labels(coll, jabs, jrel, labels)

        coll = ((c.value, *a) for c, *a in coll)
        return b''.join(bytes(c) for c in coll)


    def gen_opcode(self, declare_label, declare_position):
        """
        Given the pseudo operations added to this CodeSpace, the named
        variables declared and requested, yield the CPython opcodes
        and arguments to represent this code.
        """

        _Pseudop = Pseudop
        _Opcode = Opcode

        for op, *args in self.gen_pseudops():
            if op is _Pseudop.POSITION:
                declare_position(*args)

            elif op is _Pseudop.CALL:
                yield _Opcode.CALL_FUNCTION, args[0], args[1]

            elif op is _Pseudop.CALL_KW:
                yield _Opcode.CALL_FUNCTION_KW, args[0], args[1]

            elif op is _Pseudop.CALL_VAR:
                yield _Opcode.CALL_FUNCTION_VAR, args[0], args[1]

            elif op is _Pseudop.CALL_VAR_KW:
                yield _Opcode.CALL_FUNCTION_VAR_KW, args[0], args[1]

            elif op is _Pseudop.UNPACK_SEQUENCE:
                yield _Opcode.UNPACK_SEQUENCE, args[0], 0

            elif op is _Pseudop.UNPACK_EX:
                yield _Opcode.UNPACK_EX, args[0], args[1]

            elif op is _Pseudop.CONST:
                i = _const_index(self.consts, args[0])
                yield _Opcode.LOAD_CONST, i, 0

            elif op is _Pseudop.LOAD_CELL:
                n = args[0]
                if n in self.cell_vars:
                    i = self.cell_vars.index(n)
                    yield _Opcode.LOAD_CLOSURE, i, 0
                elif n in self.free_vars:
                    i = self.free_vars.index(n) + len(self.cell_vars)
                    yield _Opcode.LOAD_CLOSURE, i, 0
                else:
                    assert False, "missing cell name %r" % n

            elif op is _Pseudop.SET_LOCAL:
                n = args[0]
                if n in self.cell_vars:
                    i = self.cell_vars.index(n)
                    yield _Opcode.STORE_DEREF, i, 0
                elif n in self.free_vars:
                    i = self.free_vars.index(n) + len(self.cell_vars)
                    yield _Opcode.STORE_DEREF, i, 0
                elif n in self.fast_vars:
                    i = self.fast_vars.index(n)
                    yield _Opcode.STORE_FAST, i, 0
                else:
                    assert False, "missing local name %r" % n

            elif op is _Pseudop.GET_VAR:
                n = args[0]
                if n in self.cell_vars:
                    i = self.cell_vars.index(n)
                    yield _Opcode.LOAD_DEREF, i, 0
                elif n in self.free_vars:
                    i = self.free_vars.index(n) + len(self.cell_vars)
                    yield _Opcode.LOAD_DEREF, i, 0
                elif n in self.fast_vars:
                    i = self.fast_vars.index(n)
                    yield _Opcode.LOAD_FAST, i, 0
                elif n in self.global_vars:
                    i = self.names.index(n)
                    yield _Opcode.LOAD_GLOBAL, i, 0
                else:
                    assert False, "missing var %r" % n

            elif op is _Pseudop.SET_VAR:
                n = args[0]
                if n in self.cell_vars:
                    i = self.cell_vars.index(n)
                    yield _Opcode.STORE_DEREF, i, 0
                elif n in self.free_vars:
                    i = self.free_vars.index(n) + len(self.cell_vars)
                    yield _Opcode.STORE_DEREF, i, 0
                elif n in self.fast_vars:
                    i = self.fast_vars.index(n)
                    yield _Opcode.STORE_FAST, i, 0
                elif n in self.global_vars:
                    i = self.names.index(n)
                    yield _Opcode.STORE_GLOBAL, i, 0
                else:
                    assert False, "missing var %r" % n

            elif op is _Pseudop.DEL_VAR:
                n = args[0]
                if n in self.cell_vars:
                    i = self.cell_vars.index(n)
                    yield _Opcode.DELETE_DEREF, i, 0
                elif n in self.free_vars:
                    i = self.free_vars.index(n) + len(self.cell_vars)
                    yield _Opcode.DELETE_DEREF, i, 0
                elif n in self.fast_vars:
                    i = self.fast_vars.index(n)
                    yield _Opcode.DELETE_FAST, i, 0
                elif n in self.global_vars:
                    i = self.names.index(n)
                    yield _Opcode.DELETE_GLOBAL, i, 0
                else:
                    assert False, "missing var %r" % n

            elif op is _Pseudop.GET_GLOBAL:
                n = args[0]
                if n in self.global_vars:
                    i = self.names.index(n)
                    yield _Opcode.LOAD_GLOBAL, i, 0
                else:
                    assert False, "missing global name %r" % n

            elif op is _Pseudop.SET_GLOBAL:
                n = args[0]
                if n in self.global_vars:
                    i = self.names.index(n)
                    yield _Opcode.STORE_GLOBAL, i, 0
                else:
                    assert False, "missing global name %r" % n

            elif op is _Pseudop.DEL_GLOBAL:
                n = args[0]
                if n in self.global_vars:
                    i = self.names.index(n)
                    yield _Opcode.DELETE_GLOBAL, i, 0
                else:
                    assert False, "missing global name %r" % n

            elif op is _Pseudop.GET_ATTR:
                n = args[0]
                i = self.names.index(n)
                yield _Opcode.LOAD_ATTR, i, 0

            elif op is _Pseudop.SET_ATTR:
                n = args[0]
                i = self.names.index(n)
                yield _Opcode.STORE_ATTR, i, 0

            elif op is _Pseudop.DEL_ATTR:
                n = args[0]
                i = self.names.index(n)
                yield _Opcode.DELETE_ATTR, i, 0

            elif op is _Pseudop.POP:
                yield _Opcode.POP_TOP,

            # elif op is _Pseudop.MAGIC_POP_ALL:
            #     n = args[0]
            #     for _ in range(0, n):
            #         yield _Opcode.POP_TOP,

            elif op is _Pseudop.IMPORT_NAME:
                n = args[0]
                i = self.names.index(n)
                yield _Opcode.IMPORT_NAME, i, 0

            elif op is _Pseudop.IMPORT_FROM:
                n = args[0]
                i = self.names.index(n)
                yield _Opcode.IMPORT_FROM, i, 0

            elif op is _Pseudop.LAMBDA:
                yield from self.helper_gen_lambda(*args)

            elif op is _Pseudop.RET_VAL:
                yield _Opcode.RETURN_VALUE,

            elif op is _Pseudop.YIELD_VAL:
                yield _Opcode.YIELD_VALUE,

            elif op is _Pseudop.YIELD_FROM:
                yield _Opcode.YIELD_FROM,

            elif op is _Pseudop.DUP:
                yield _Opcode.DUP_TOP,

            elif op is _Pseudop.LABEL:
                declare_label(args[0])

            elif op in (_Pseudop.FAUX_PUSH,
                        _Pseudop.DEBUG_STACK):
                pass

            elif op is _Pseudop.JUMP:
                yield _Opcode.JUMP_ABSOLUTE, args[0], 0

            elif op is _Pseudop.JUMP_FORWARD:
                yield _Opcode.JUMP_FORWARD, args[0], 0

            elif op is _Pseudop.POP_JUMP_IF_TRUE:
                yield _Opcode.POP_JUMP_IF_TRUE, args[0]

            elif op is _Pseudop.POP_JUMP_IF_FALSE:
                yield _Opcode.POP_JUMP_IF_FALSE, args[0]

            elif op is _Pseudop.BUILD_TUPLE:
                yield _Opcode.BUILD_TUPLE, args[0], 0

            elif op is _Pseudop.BUILD_TUPLE_UNPACK:
                yield _Opcode.BUILD_TUPLE_UNPACK, args[0], 0

            elif op is _Pseudop.BUILD_MAP:
                yield _Opcode.BUILD_MAP, args[0], 0

            elif op is _Pseudop.BUILD_MAP_UNPACK:
                yield _Opcode.BUILD_MAP_UNPACK, args[0], 0

            elif op is _Pseudop.BUILD_LIST:
                yield _Opcode.BUILD_LIST, args[0], 0

            elif op is _Pseudop.BUILD_SET:
                yield _Opcode.BUILD_SET, args[0], 0

            elif op is _Pseudop.BUILD_SLICE:
                yield _Opcode.BUILD_SLICE, args[0], 0

            elif op is _Pseudop.SETUP_WITH:
                yield _Opcode.SETUP_WITH, args[0], 0

            elif op is _Pseudop.WITH_CLEANUP_START:
                yield _Opcode.WITH_CLEANUP_START,

            elif op is _Pseudop.WITH_CLEANUP_FINISH:
                yield _Opcode.WITH_CLEANUP_FINISH,

            elif op is _Pseudop.SETUP_EXCEPT:
                yield _Opcode.SETUP_EXCEPT, args[0], 0

            elif op is _Pseudop.SETUP_FINALLY:
                yield _Opcode.SETUP_FINALLY, args[0], 0

            elif op is _Pseudop.SETUP_LOOP:
                yield _Opcode.SETUP_LOOP, args[0], 0

            elif op is _Pseudop.POP_BLOCK:
                yield _Opcode.POP_BLOCK,

            elif op is _Pseudop.POP_EXCEPT:
                yield _Opcode.POP_EXCEPT,

            elif op is _Pseudop.END_FINALLY:
                yield _Opcode.END_FINALLY,

            elif op is _Pseudop.UNARY_POSITIVE:
                yield _Opcode.UNARY_POSITIVE,

            elif op is _Pseudop.UNARY_NEGATIVE:
                yield _Opcode.UNARY_NEGATIVE,

            elif op is _Pseudop.UNARY_NOT:
                yield _Opcode.UNARY_NOT,

            elif op is _Pseudop.UNARY_INVERT:
                yield _Opcode.UNARY_INVERT,

            elif op is _Pseudop.ITER:
                yield _Opcode.GET_ITER,

            elif op is _Pseudop.FOR_ITER:
                yield _Opcode.FOR_ITER, args[0]

            elif op is _Pseudop.GET_YIELD_FROM_ITER:
                yield _Opcode.GET_YIELD_FROM_ITER,

            elif op is _Pseudop.COMPARE_OP:
                yield _Opcode.COMPARE_OP, args[0], 0

            elif op is _Pseudop.GET_ITEM:
                yield _Opcode.BINARY_SUBSCR,

            elif op is _Pseudop.SET_ITEM:
                yield _Opcode.STORE_SUBSCR,

            elif op is _Pseudop.DEL_ITEM:
                yield _Opcode.DELETE_SUBSCR,

            elif op is _Pseudop.BINARY_ADD:
                yield _Opcode.BINARY_ADD,

            elif op is _Pseudop.BINARY_SUBTRACT:
                yield _Opcode.BINARY_SUBTRACT,

            elif op is _Pseudop.BINARY_MULTIPLY:
                yield _Opcode.BINARY_MULTIPLY,

            elif op is _Pseudop.BINARY_MATRIX_MULTIPLY:
                yield _Opcode.BINARY_MATRIX_MULTIPLY,

            elif op is _Pseudop.BINARY_TRUE_DIVIDE:
                yield _Opcode.BINARY_TRUE_DIVIDE,

            elif op is _Pseudop.BINARY_FLOOR_DIVIDE:
                yield _Opcode.BINARY_FLOOR_DIVIDE,

            elif op is _Pseudop.BINARY_POWER:
                yield _Opcode.BINARY_POWER,

            elif op is _Pseudop.BINARY_MODULO:
                yield _Opcode.BINARY_MODULO,

            elif op is _Pseudop.BINARY_LSHIFT:
                yield _Opcode.BINARY_LSHIFT,

            elif op is _Pseudop.BINARY_RSHIFT:
                yield _Opcode.BINARY_RSHIFT,

            elif op is _Pseudop.BINARY_AND:
                yield _Opcode.BINARY_AND,

            elif op is _Pseudop.BINARY_XOR:
                yield _Opcode.BINARY_XOR,

            elif op is _Pseudop.BINARY_OR:
                yield _Opcode.BINARY_OR,

            elif op is _Pseudop.RAISE:
                yield _Opcode.RAISE_VARARGS, args[0], 0

            elif op is _Pseudop.ROT_TWO:
                yield _Opcode.ROT_TWO,

            elif op is _Pseudop.ROT_THREE:
                yield _Opcode.ROT_THREE,

            elif op is _Pseudop.CONTINUE_LOOP:
                yield _Opcode.CONTINUE_LOOP, args[0]

            elif op is _Pseudop.BREAK_LOOP:
                yield _Opcode.BREAK_LOOP,

            else:
                assert False, "Unknown Pseudop %r" % op


    def pseudop_build_str(self, count):
        # emulate the BUILD_STRING opcode using string joining

        self.pseudop_build_tuple(count)

        self.pseudop_const("")
        self.pseudop_get_attr(symbol("join"))
        self.pseudop_rot_two()
        self.pseudop_call(1)


    def pseudop_format(self, flags):
        # emulate the FORMAT_VALUE opcode using the format function

        # TOS format str if flags & 0x01
        # TOS2 value

        self.pseudop_get_global(symbol("format"))

        if flags & 0x04:
            self.pseudop_rot_three()
            self.pseudop_call(2)
        else:
            self.pseudop_rot_two()
            self.pseudop_call(1)


    def pseudop_lambda(self, code, defaults=(), kwonly=()):
        for arg, expr in defaults:
            self.add_expression(expr)

        for arg, expr in kwonly:
            self.pseudop_const(str(arg))
            self.add_expression(expr)

        super().pseudop_lambda(code, len(defaults), len(kwonly))


    def helper_gen_lambda(self, code, default_count, kwonly_count):
        """
        Helper to _gen_code that handles just lambda definitions
        """

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


    def helper_compile_call(self, args, declared_at):
        params = gather_parameters(args)

        pos, keywords, values, vargs, vkwds = params

        assert (len(keywords) == len(values)), "mismatched keyword, values"

        for expr in pos:
            self.add_expression(expr)

        if vargs:
            self.add_expression(vargs)

        for key, val in zip(keywords, values):
            self.pseudop_const(str(key))
            self.add_expression(val)

        if vkwds:
            self.add_expression(vkwds)

        if vargs:
            if vkwds:
                # CALL_FUNCTION_VAR_KW
                pseu = self.pseudop_call_var_kw
            else:
                # CALL_FUNCTION_VAR
                pseu = self.pseudop_call_var
        else:
            if vkwds:
                # CALL_FUNCTION_KW
                pseu = self.pseudop_call_kw
            else:
                # CALL_FUNCTION
                pseu = self.pseudop_call

        if declared_at:
            self.pseudop_position(*declared_at)

        pseu(len(pos), len(keywords))


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


    def helper_max_stack(self, op, args, push, pop):

        _Pseudop = Pseudop

        if op is _Pseudop.CALL:
            pop(args[0])      # positionals
            pop(args[1] * 2)  # kw:val pairs
            pop()             # function
            push()            # result

        elif op is _Pseudop.CALL_KW:
            pop(args[0])      # positionals
            pop(args[1] * 2)  # kw:val pairs
            pop(2)            # kwds, function
            push()            # result

        elif op is _Pseudop.CALL_VAR:
            pop(args[0])      # positionals
            pop(args[1] * 2)  # kw:val pairs
            pop(2)            # args, function
            push()            # result

        elif op is _Pseudop.CALL_VAR_KW:
            pop(args[0])      # positionals
            pop(args[1] * 2)  # kw:val pairs
            pop(3)            # args, kwds, function
            push()            # result

        elif op is _Pseudop.LAMBDA:
            pop(args[1])
            pop(args[2] * 2)
            push()

        else:
            return super().helper_max_stack(op, args, push, pop)


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


def apply_jump_labels(coll, jabs, jrel, labels):
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


#
# The end.
