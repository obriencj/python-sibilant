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
sibilant.pseudops.stack

Stack counting utilities for sibiliant pseudops blocks

author: Christopher O'Brien <obriencj@gmail.com>
license: LGPL v.3
"""


from . import Pseudop, Opcode
from ..lib import SibilantException


__all__ = (
    "StackCounter", "StackUnderrun", "StackMismatch",
    "stacker", "stacking",
    "stacking_pop_args_push", "stacking_push_args_pop",
)


class StackUnderrun(SibilantException):
    def __init__(self, stack_size, pop_size):

        self.start_size = stack_size
        self.pop_size = pop_size

        msg = "stack size underrun, %i decremented by %i"
        super().__init__(msg % (stack_size, pop_size))


class StackMismatch(SibilantException):
    def __init__(self, label, label_size, jump_size):

        self.label = label
        self.label_size = label_size
        self.jump_size = jump_size

        msg = "mismatch between label %r stack: %i and origin stack: %i"
        super().__init__(msg % (label, label_size, jump_size))


class MetaStackCounter(type):

    def __init__(self, name, bases, members):

        # the _translations_ member is merged together with those of
        # the bases, to provide a sort of inheritance.
        stk = {}
        for base in bases:
            stk.update(getattr(base, "_stackers_", {}))
        stk.update(members.get("_stackers_", {}))

        # we also hunt for decorated callable members to inject into
        # the _stackers_
        for memb in members.values():
            if callable(memb):
                trop = getattr(memb, "_stacks_", None)
                if trop is not None:
                    stk[trop] = memb

        members["_stackers_"] = stk
        self._stackers_ = stk

        return super().__init__(name, bases, members)


def stacker(pseudop):
    def decorator(member):
        member._stacks_ = pseudop
        return member
    return decorator


def stacking(pushc, popc):
    def pop_then_push(self, pseudop, args, push, pop):
        if popc:
            pop(popc)
        if pushc:
            push(pushc)

    return pop_then_push


def stacking_pop_args_push(pushc):
    def pop_args_then_push(self, pseudop, args, push, pop):
        pop(args[0])
        if pushc:
            push(pushc)

    return pop_args_then_push


def stacking_push_args_pop(popc):
    def pop_then_push_args(self, pseudops, args, push, pop):
        if popc:
            pop(popc)
        push(args[0])

    return pop_then_push_args


class StackCounter(metaclass=MetaStackCounter):


    _stackers_ = {
        Pseudop.POSITION: stacking(0, 0),
        Pseudop.DEL_VAR: stacking(0, 0),
        Pseudop.DEL_GLOBAL: stacking(0, 0),
        Pseudop.ROT_TWO: stacking(0, 0),
        Pseudop.ROT_THREE: stacking(0, 0),

        Pseudop.CONST: stacking(1, 0),
        Pseudop.DUP: stacking(1, 0),
        Pseudop.GET_VAR: stacking(1, 0),
        Pseudop.GET_GLOBAL: stacking(1, 0),
        Pseudop.BREAK_LOOP: stacking(1, 0),
        Pseudop.FOR_ITER: stacking(1, 0),
        Pseudop.IMPORT_FROM: stacking(1, 0),
        Pseudop.CONTINUE_LOOP: stacking(1, 0),
        Pseudop.LOAD_CELL: stacking(1, 0),

        Pseudop.GET_ATTR: stacking(1, 1),
        Pseudop.UNARY_POSITIVE: stacking(1, 1),
        Pseudop.UNARY_NEGATIVE: stacking(1, 1),
        Pseudop.UNARY_NOT: stacking(1, 1),
        Pseudop.UNARY_INVERT: stacking(1, 1),
        Pseudop.ITER: stacking(1, 1),
        Pseudop.GET_YIELD_FROM_ITER: stacking(1, 1),
        Pseudop.YIELD_VAL: stacking(1, 1),

        Pseudop.COMPARE_OP: stacking(1, 2),
        Pseudop.GET_ITEM: stacking(1, 2),
        Pseudop.BINARY_ADD: stacking(1, 2),
        Pseudop.BINARY_SUBTRACT: stacking(1, 2),
        Pseudop.BINARY_MULTIPLY: stacking(1, 2),
        Pseudop.BINARY_MATRIX_MULTIPLY: stacking(1, 2),
        Pseudop.BINARY_TRUE_DIVIDE: stacking(1, 2),
        Pseudop.BINARY_FLOOR_DIVIDE: stacking(1, 2),
        Pseudop.BINARY_POWER: stacking(1, 2),
        Pseudop.BINARY_MODULO: stacking(1, 2),
        Pseudop.BINARY_LSHIFT: stacking(1, 2),
        Pseudop.BINARY_RSHIFT: stacking(1, 2),
        Pseudop.BINARY_AND: stacking(1, 2),
        Pseudop.BINARY_XOR: stacking(1, 2),
        Pseudop.BINARY_OR: stacking(1, 2),
        Pseudop.IMPORT_NAME: stacking(1, 2),

        Pseudop.SET_ATTR: stacking(0, 2),
        Pseudop.DEL_ITEM: stacking(0, 2),

        Pseudop.SET_ITEM: stacking(0, 3),

        Pseudop.POP: stacking(0, 1),
        Pseudop.DEL_ATTR: stacking(0, 1),
        Pseudop.SET_GLOBAL: stacking(0, 1),
        Pseudop.SET_LOCAL: stacking(0, 1),
        Pseudop.SET_VAR: stacking(0, 1),
        Pseudop.RET_VAL: stacking(0, 1),
        Pseudop.YIELD_FROM: stacking(0, 1),

        Pseudop.SETUP_EXCEPT: stacking(6, 0),
        Pseudop.POP_EXCEPT: stacking(0, 3),

        Pseudop.SETUP_WITH: stacking(6, 0),
        Pseudop.WITH_CLEANUP_START: stacking(7, 0),
        Pseudop.WITH_CLEANUP_FINISH: stacking(0, 7),

        Pseudop.SETUP_FINALLY: stacking(6, 0),
        Pseudop.END_FINALLY: stacking(0, 6),

        Pseudop.BUILD_LIST: stacking_pop_args_push(1),
        Pseudop.BUILD_SET: stacking_pop_args_push(1),
        Pseudop.BUILD_TUPLE: stacking_pop_args_push(1),
        Pseudop.BUILD_TUPLE_UNPACK: stacking_pop_args_push(1),
        Pseudop.BUILD_MAP_UNPACK: stacking_pop_args_push(1),
        Pseudop.BUILD_SLICE: stacking_pop_args_push(1),
        Pseudop.RAISE: stacking_pop_args_push(1),
        Pseudop.FAUX_POP: stacking_pop_args_push(0),

        Pseudop.UNPACK_SEQUENCE: stacking_push_args_pop(1),
        Pseudop.FAUX_PUSH: stacking_push_args_pop(0),
    }


    # @stacker(Pseudop.LAMBDA)
    # def stacks_lambda(self, pseudop, args, push, pop):
    #    pop(args[1])
    #    a = len(args[0].co_freevars)
    #    if a:
    #        push(a)
    #        pop(a)
    #    push(2)
    #    pop(2)
    #    push()


    @stacker(Pseudop.BUILD_MAP)
    def stacker_build_map(self, pseudop, args, push, pop):
        pop(args[0] * 2)
        push()


    @stacker(Pseudop.UNPACK_EX)
    def stacker_unpack_ex(self, pseudop, args, push, pop):
        pop()
        push(args[0] + args[1] + 1)


    @stacker(Pseudop.SETUP_LOOP)
    def stacker_setup_loop(self, pseudop, args, push, pop):
        push(Opcode.SETUP_LOOP.stack_effect(1))


    @stacker(Pseudop.POP_BLOCK)
    def stacker_pop_block(self, pseudop, args, push, pop):
        push(Opcode.POP_BLOCK.stack_effect())


    @stacker(Pseudop.LABEL)
    def stacker_label(self, pseudop, args, push, pop):
        stac = self.stack_count
        stac = self.labels.setdefault(args[0], stac)
        self.stack_count = stac


    @stacker(Pseudop.BLOCK)
    def stacker_block(self, pseudop, args, push, pop):
        block = args[0]
        block_i, block_max = block.max_stack(self.compiler)
        push(block_max)
        pop(block_max)


    @stacker(Pseudop.JUMP)
    def stacker_jump(self, pseudop, args, push, pop):
        stac = self.stack_count
        dest = args[0]

        check = self.labels.get(dest, None)

        if check is None:
            self.labels[dest] = stac

        elif check != stac:
            raise StackMismatch(dest, check, stac)


    @stacker(Pseudop.JUMP_FORWARD)
    def stacker_jump_forward(self, pseudop, args, push, pop):
        self.stacker_jump(pseudop, args, push, pop)


    @stacker(Pseudop.POP_JUMP_IF_TRUE)
    def stacker_pop_jump_if_true(self, pseudop, args, push, pop):
        pop()
        self.stacker_jump(pseudop, args, push, pop)


    @stacker(Pseudop.POP_JUMP_IF_FALSE)
    def stacker_pop_jump_if_false(self, pseudop, args, push, pop):
        pop()
        self.stacker_jump(pseudop, args, push, pop)


    @stacker(Pseudop.DEBUG_STACK)
    def stacker_debug_stack(self, pseudop, args, push, pop):
        print(" ".join(map(str, args)),
              "max:", self.max_stack,
              "cur:", self.stack_count)


    def __init__(self, compiler, start_size=0):
        super().__init__()
        self.compiler = compiler
        self.stack_count = start_size
        self.max_stack = start_size
        self.labels = {}


    def stack(self, pseudop, args):
        handler = self._stackers_.get(pseudop, None)
        assert (handler is not None), ("no stacker for %r" % pseudop)
        return handler(self, pseudop, args, self.push, self.pop)


    def push(self, by=1):
        stac = self.stack_count
        maxc = self.max_stack

        stac += by
        if stac > maxc:
            maxc = stac

        self.stack_count = stac
        self.max_stack = maxc


    def pop(self, by=1):
        stac = self.stack_count
        maxc = self.max_stack

        stac -= by
        if stac < 0:
            raise StackUnderrun(self.stack_count, by)

        self.stack_count = stac
        self.max_stack = maxc


#
# The end.
