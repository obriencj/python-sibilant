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


from types import CodeType

from .. import Block
from .cpython37 import PseudopsCPython37, StackCounterCPython37


class PseudopsCPython38(PseudopsCPython37):
    """
    Pseudops compiler emitting bytecode compatible with CPython version
    3.8
    """


    def _codetype(self,
                  argcount=0, posonlyargcount=0, kwonlyargcount=0,
                  nlocals=0,
                  stacksize=0, flags=0, codestring=None,
                  constants=(), names=(), varnames=(),
                  filename=None, name="<anon>",
                  firstlineno=1, lnotab=(),
                  freevars=(), cellvars=()):

        # this indirection is necessary because the CodeType
        # parameters changes in later versions

        # note: posonlyargcount is 3.8+

        return CodeType(argcount,
                        posonlyargcount,
                        kwonlyargcount,
                        nlocals,
                        stacksize,
                        flags,
                        codestring,
                        constants,
                        names,
                        varnames,
                        filename,
                        name,
                        firstlineno,
                        lnotab,
                        freevars,
                        cellvars)


    def stack_counter(self, start_size=0):
        return StackCounterCPython38(self, start_size)


    def pseudop_setup_loop(self, label):
        pass


    def pseudop_break_loop(self):
        block = self.get_block()

        while True:
            if block.block_type == Block.LOOP:
                stack_count = block.current_stack(self)
                break

            elif block.block_type == Block.TRY:
                self.pseudop_pop_block()
                block = block.parent()

            elif block.block_type == Block.EXCEPT:
                self.pseudop_pop_finally()
                block = block.parent()

            elif block.block_type == Block.BASIC:
                stack_count = 0
                break

        self.pseudop_pop(stack_count)
        self.pseudop_jump(block.pop_label)


    def pseudop_continue_loop(self):
        block = self.get_block()

        while True:
            if block.block_type == Block.LOOP:
                stack_count = block.current_stack(self)
                break

            elif block.block_type == Block.TRY:
                self.pseudop_pop_block()
                block = block.parent()

            elif block.block_type == Block.EXCEPT:
                self.pseudop_pop_finally()
                block = block.parent()

            elif block.block_type == Block.BASIC:
                stack_count = 0
                break

        self.pseudop_pop(stack_count)
        self.pseudop_jump(block.top_label)


class StackCounterCPython38(StackCounterCPython37):
    pass


#
# The end.
