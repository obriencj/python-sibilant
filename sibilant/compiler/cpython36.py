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
sibilant.compiler.cpython36

Compiler target for CPython 3.6 bytecode

author: Christopher O'Brien <obriencj@gmail.com>
license: LGPL v.3
"""


from . import (
    ExpressionCodeSpace, Pseudop, Opcode,
    gather_parameters,
)


class CPython36(ExpressionCodeSpace):
    """
    SpecialCodeSpace emitting bytecode compatible with CPython version
    3.6
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
                coll.append(opa)
                offset += 2

        if jabs or jrel or labels:
            # Given our labels, modify jmp calls to point to the label
            apply_jump_labels(coll, jabs, jrel, labels)

        coll = ((c.value, a) for c, a in coll)
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
                yield _Opcode.CALL_FUNCTION, args[0]

            elif op is _Pseudop.CALL_KW:
                yield _Opcode.CALL_FUNCTION_KW, args[0]

            elif op is _Pseudop.CALL_VAR:
                yield _Opcode.CALL_FUNCTION_EX, 0x00

            elif op is _Pseudop.CALL_VAR_KW:
                yield _Opcode.CALL_FUNCTION_EX, 0x01

            elif op is _Pseudop.UNPACK_SEQUENCE:
                yield _Opcode.UNPACK_SEQUENCE, args[0]

            elif op is _Pseudop.UNPACK_EX:
                if args[1]:
                    yield _Opcode.EXTENDED_ARG, args[1]
                yield _Opcode.UNPACK_EX, args[0]

            elif op is _Pseudop.CONST:
                i = _const_index(self.consts, args[0])
                yield _Opcode.LOAD_CONST, i

            elif op is _Pseudop.SET_LOCAL:
                n = args[0]
                if n in self.cell_vars:
                    i = self.cell_vars.index(n)
                    yield _Opcode.STORE_DEREF, i
                elif n in self.free_vars:
                    i = self.free_vars.index(n) + len(self.cell_vars)
                    yield _Opcode.STORE_DEREF, i
                elif n in self.fast_vars:
                    i = self.fast_vars.index(n)
                    yield _Opcode.STORE_FAST, i
                else:
                    assert False, "missing local name %r" % n

            elif op is _Pseudop.GET_VAR:
                n = args[0]
                if n in self.cell_vars:
                    i = self.cell_vars.index(n)
                    yield _Opcode.LOAD_DEREF, i
                elif n in self.free_vars:
                    i = self.free_vars.index(n) + len(self.cell_vars)
                    yield _Opcode.LOAD_DEREF, i
                elif n in self.fast_vars:
                    i = self.fast_vars.index(n)
                    yield _Opcode.LOAD_FAST, i
                elif n in self.global_vars:
                    i = self.names.index(n)
                    yield _Opcode.LOAD_GLOBAL, i
                else:
                    assert False, "missing var %r" % n

            elif op is _Pseudop.SET_VAR:
                n = args[0]
                if n in self.cell_vars:
                    i = self.cell_vars.index(n)
                    yield _Opcode.STORE_DEREF, i
                elif n in self.free_vars:
                    i = self.free_vars.index(n) + len(self.cell_vars)
                    yield _Opcode.STORE_DEREF, i
                elif n in self.fast_vars:
                    i = self.fast_vars.index(n)
                    yield _Opcode.STORE_FAST, i
                elif n in self.global_vars:
                    i = self.names.index(n)
                    yield _Opcode.STORE_GLOBAL, i
                else:
                    assert False, "missing var %r" % n

            elif op is _Pseudop.DEL_VAR:
                n = args[0]
                if n in self.cell_vars:
                    i = self.cell_vars.index(n)
                    yield _Opcode.DELETE_DEREF, i
                elif n in self.free_vars:
                    i = self.free_vars.index(n) + len(self.cell_vars)
                    yield _Opcode.DELETE_DEREF, i
                elif n in self.fast_vars:
                    i = self.fast_vars.index(n)
                    yield _Opcode.DELETE_FAST, i
                elif n in self.global_vars:
                    i = self.names.index(n)
                    yield _Opcode.DELETE_GLOBAL, i
                else:
                    assert False, "missing var %r" % n

            elif op is _Pseudop.GET_GLOBAL:
                n = args[0]
                if n in self.global_vars:
                    i = self.names.index(n)
                    yield _Opcode.LOAD_GLOBAL, i
                else:
                    assert False, "missing global name %r" % n

            elif op is _Pseudop.SET_GLOBAL:
                n = args[0]
                if n in self.global_vars:
                    i = self.names.index(n)
                    yield _Opcode.STORE_GLOBAL, i
                else:
                    assert False, "missing global name %r" % n

            elif op is _Pseudop.DEL_GLOBAL:
                n = args[0]
                if n in self.global_vars:
                    i = self.names.index(n)
                    yield _Opcode.DELETE_GLOBAL, i
                else:
                    assert False, "missing global name %r" % n

            elif op is _Pseudop.GET_ATTR:
                n = args[0]
                i = self.names.index(n)
                yield _Opcode.LOAD_ATTR, i

            elif op is _Pseudop.SET_ATTR:
                n = args[0]
                i = self.names.index(n)
                yield _Opcode.STORE_ATTR, i

            elif op is _Pseudop.DEL_ATTR:
                n = args[0]
                i = self.names.index(n)
                yield _Opcode.DELETE_ATTR, i

            elif op is _Pseudop.POP:
                yield _Opcode.POP_TOP, 0

            # elif op is _Pseudop.MAGIC_POP_ALL:
            #     n = args[0]
            #     for _ in range(0, n):
            #         yield _Opcode.POP_TOP, 0

            elif op is _Pseudop.IMPORT_NAME:
                n = args[0]
                i = self.names.index(n)
                yield _Opcode.IMPORT_NAME, i

            elif op is _Pseudop.IMPORT_FROM:
                n = args[0]
                i = self.names.index(n)
                yield _Opcode.IMPORT_FROM, i

            elif op is _Pseudop.LAMBDA:
                yield from self.helper_gen_lambda(*args)

            elif op is _Pseudop.RET_VAL:
                yield _Opcode.RETURN_VALUE, 0

            elif op is _Pseudop.YIELD_VAL:
                yield _Opcode.YIELD_VALUE, 0

            elif op is _Pseudop.YIELD_FROM:
                yield _Opcode.YIELD_FROM, 0

            elif op is _Pseudop.DUP:
                yield _Opcode.DUP_TOP, 0

            elif op is _Pseudop.LABEL:
                declare_label(args[0])

            elif op in (_Pseudop.FAUX_PUSH,
                        _Pseudop.DEBUG_STACK):
                pass

            elif op is _Pseudop.JUMP:
                yield _Opcode.JUMP_ABSOLUTE, args[0]

            elif op is _Pseudop.JUMP_FORWARD:
                yield _Opcode.JUMP_FORWARD, args[0]

            elif op is _Pseudop.POP_JUMP_IF_TRUE:
                yield _Opcode.POP_JUMP_IF_TRUE, args[0]

            elif op is _Pseudop.POP_JUMP_IF_FALSE:
                yield _Opcode.POP_JUMP_IF_FALSE, args[0]

            elif op is _Pseudop.BUILD_TUPLE:
                yield _Opcode.BUILD_TUPLE, args[0]

            elif op is _Pseudop.BUILD_TUPLE_UNPACK:
                yield _Opcode.BUILD_TUPLE_UNPACK, args[0]

            elif op is _Pseudop.BUILD_MAP:
                yield _Opcode.BUILD_MAP, args[0]

            elif op is _Pseudop.BUILD_MAP_UNPACK:
                yield _Opcode.BUILD_MAP_UNPACK, args[0]

            elif op is _Pseudop.BUILD_LIST:
                yield _Opcode.BUILD_LIST, args[0]

            elif op is _Pseudop.BUILD_SET:
                yield _Opcode.BUILD_SET, args[0]

            elif op is _Pseudop.BUILD_STR:
                yield _Opcode.BUILD_STRING, args[0]

            elif op is _Pseudop.FORMAT:
                yield _Opcode.FORMAT_VALUE, args[0]

            elif op is _Pseudop.SETUP_WITH:
                yield _Opcode.SETUP_WITH, args[0]

            elif op is _Pseudop.WITH_CLEANUP_START:
                yield _Opcode.WITH_CLEANUP_START, 0

            elif op is _Pseudop.WITH_CLEANUP_FINISH:
                yield _Opcode.WITH_CLEANUP_FINISH, 0

            elif op is _Pseudop.SETUP_EXCEPT:
                yield _Opcode.SETUP_EXCEPT, args[0]

            elif op is _Pseudop.SETUP_FINALLY:
                yield _Opcode.SETUP_FINALLY, args[0]

            elif op is _Pseudop.SETUP_LOOP:
                yield _Opcode.SETUP_LOOP, args[0]

            elif op is _Pseudop.POP_BLOCK:
                yield _Opcode.POP_BLOCK, 0

            elif op is _Pseudop.POP_EXCEPT:
                yield _Opcode.POP_EXCEPT, 0

            elif op is _Pseudop.END_FINALLY:
                yield _Opcode.END_FINALLY, 0

            elif op is _Pseudop.UNARY_POSITIVE:
                yield _Opcode.UNARY_POSITIVE, 0

            elif op is _Pseudop.UNARY_NEGATIVE:
                yield _Opcode.UNARY_NEGATIVE, 0

            elif op is _Pseudop.UNARY_NOT:
                yield _Opcode.UNARY_NOT, 0

            elif op is _Pseudop.UNARY_INVERT:
                yield _Opcode.UNARY_INVERT, 0

            elif op is _Pseudop.ITER:
                yield _Opcode.GET_ITER, 0

            elif op is _Pseudop.FOR_ITER:
                yield _Opcode.FOR_ITER, args[0]

            elif op is _Pseudop.GET_YIELD_FROM_ITER:
                yield _Opcode.GET_YIELD_FROM_ITER, 0

            elif op is _Pseudop.COMPARE_OP:
                yield _Opcode.COMPARE_OP, args[0]

            elif op is _Pseudop.GET_ITEM:
                yield _Opcode.BINARY_SUBSCR, 0

            elif op is _Pseudop.SET_ITEM:
                yield _Opcode.STORE_SUBSCR, 0

            elif op is _Pseudop.DEL_ITEM:
                yield _Opcode.DELETE_SUBSCR, 0

            elif op is _Pseudop.BINARY_ADD:
                yield _Opcode.BINARY_ADD, 0

            elif op is _Pseudop.BINARY_SUBTRACT:
                yield _Opcode.BINARY_SUBTRACT, 0

            elif op is _Pseudop.BINARY_MULTIPLY:
                yield _Opcode.BINARY_MULTIPLY, 0

            elif op is _Pseudop.BINARY_MATRIX_MULTIPLY:
                yield _Opcode.BINARY_MATRIX_MULTIPLY, 0

            elif op is _Pseudop.BINARY_TRUE_DIVIDE:
                yield _Opcode.BINARY_TRUE_DIVIDE, 0

            elif op is _Pseudop.BINARY_FLOOR_DIVIDE:
                yield _Opcode.BINARY_FLOOR_DIVIDE, 0

            elif op is _Pseudop.BINARY_POWER:
                yield _Opcode.BINARY_POWER, 0

            elif op is _Pseudop.BINARY_MODULO:
                yield _Opcode.BINARY_MODULO, 0

            elif op is _Pseudop.BINARY_LSHIFT:
                yield _Opcode.BINARY_LSHIFT, 0

            elif op is _Pseudop.BINARY_RSHIFT:
                yield _Opcode.BINARY_RSHIFT, 0

            elif op is _Pseudop.BINARY_AND:
                yield _Opcode.BINARY_AND, 0

            elif op is _Pseudop.BINARY_XOR:
                yield _Opcode.BINARY_XOR, 0

            elif op is _Pseudop.BINARY_OR:
                yield _Opcode.BINARY_OR, 0

            elif op is _Pseudop.RAISE:
                yield _Opcode.RAISE_VARARGS, args[0]

            elif op is _Pseudop.ROT_TWO:
                yield _Opcode.ROT_TWO, 0

            elif op is _Pseudop.ROT_THREE:
                yield _Opcode.ROT_THREE, 0

            elif op is _Pseudop.CONTINUE_LOOP:
                yield _Opcode.CONTINUE_LOOP, args[0]

            elif op is _Pseudop.BREAK_LOOP:
                yield _Opcode.BREAK_LOOP, 0

            else:
                assert False, "Unknown Pseudop %r" % op


    def pseudop_lambda(self, code, defaults=(), kwonly=()):

        if defaults:
            for arg, expr in defaults:
                self.add_expression(expr)
            self.pseudop_build_tuple(len(defaults))

        if kwonly:
            for arg, expr in kwonly:
                self.pseudop_const(str(arg))
                self.add_expression(expr)
            self.pseudop_build_map(len(kwonly))

        super().pseudop_lambda(code, len(defaults), len(kwonly))


    def helper_gen_lambda(self, code, default_count, kwonly_count):
        """
        Helper to _gen_code that handles just lambda definitions
        """

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
                if f in self.cell_vars:
                    fi = self.cell_vars.index(f)
                elif f in self.free_vars:
                    fi = len(self.cell_vars)
                    fi += self.free_vars.index(f)
                else:
                    assert False, "missing local var %r" % f
                yield _Opcode.LOAD_CLOSURE, fi

            yield _Opcode.BUILD_TUPLE, len(code.co_freevars)
            flags |= 0x08

        # not a closure, so just a pain ol' function
        yield _Opcode.LOAD_CONST, ci
        yield _Opcode.LOAD_CONST, ni
        yield _Opcode.MAKE_FUNCTION, flags


    def helper_compile_call(self, args, declared_at):
        params = gather_parameters(args)

        pos, keywords, values, vargs, vkwds = params

        assert (len(keywords) == len(values)), "mismatched keyword, values"

        arg_tuple = 0

        # step one, evaluate the positionals that we have
        for expr in pos:
            self.add_expression(expr)

        if not (vargs or keywords or vkwds):
            # easy mode, nothing fancy, just a plain 'ol call
            self.pseudop_call(len(pos))
            return

        elif pos:
            # it's going to get complicated. collect the positionals
            # we've got into a tuple for later.
            self.pseudop_build_tuple(len(pos))
            arg_tuple += 1

        if vargs:
            # another tuple of positional arguments onto the pile
            self.add_expression(vargs)
            arg_tuple += 1

        if arg_tuple > 1:
            # if we have more than one positional tuple, join them
            # together into a single tuple (or if we have none, create
            # an empty tuple)
            self.pseudop_build_tuple_unpack(arg_tuple)

        if not (keywords or vkwds):
            # just positionals, so invoke CALL_FUNCTION_EX 0x00
            self.pseudop_call_var(0)
            return

        elif not arg_tuple:
            # in order to support CALL_FUNCTION_EX later on, we're
            # going to push an empty tuple on, to represent our lack
            # of positionals
            self.pseudop_build_tuple(0)

        kwd_tuple = 0
        if keywords:
            # build a map out of all the keyword:value entries
            for key, val in zip(keywords, values):
                self.pseudop_const(str(key))
                self.add_expression(val)
            self.pseudop_build_map(len(keywords))
            kwd_tuple += 1

        if vkwds:
            # if we also have a variadic kwd, grab that.
            self.add_expression(vkwds)
            kwd_tuple += 1

        if kwd_tuple > 1:
            # if we have both keywords and variadic kwds, join' em together
            self.pseudop_build_map_unpack(kwd_tuple)

        if declared_at:
            self.pseudop_position(*declared_at)

        # even if we had no positionals, we've created an empty
        # positionals tuple, and now we can CALL_FUNCTION_EX 0x01
        self.pseudop_call_var_kw(0)


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


    def helper_max_stack(self, op, args, push, pop):

        _Pseudop = Pseudop

        if op is _Pseudop.CALL:
            pop(args[0] + 1)  # positionals, function
            push()  # result

        elif op is _Pseudop.CALL_VAR:
            pop(2)  # args, function
            push()  # result

        elif op is _Pseudop.CALL_VAR_KW:
            pop(3)  # kwargs, args, function
            push()  # result

        elif op is _Pseudop.LAMBDA:
            if args[1]:
                pop()
            if args[2]:
                pop()
            push()

        elif op is _Pseudop.FORMAT:
            pop()
            if args[0] & 0x04:
                pop()
            push()

        else:
            return super().helper_max_stack(op, args, push, pop)


def _const_index(of_list, value):
    # have to use an `is` comparator, because the list.index method
    # will consider False and 0, and True and 1 to be equivalent,
    # breaking any constant pools containing those values.

    for index, found in enumerate(of_list):
        if found is value:
            return index
    else:
        assert False, "missing constant pool index for value %r" % value


def apply_jump_labels(coll, jabs, jrel, labels):
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


#
# The end.
