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


from sibilant.pseudops import Opcode, Pseudop, translator
from sibilant.pseudops.stack import stacker
from .cpython36 import PseudopsCPython36, StackCounterCPython36


class PseudopsCPython37(PseudopsCPython36):
    """
    Pseudops compiler emitting bytecode compatible with CPython
    version 3.7
    """


    def pseudop_get_method(self, namesym):
        self.request_name(namesym)
        return self.pseudop(Pseudop.GET_METHOD, namesym)


    def pseudop_call_method(self, argc):
        return self.pseudop(Pseudop.CALL_METHOD, argc)


    @translator(Pseudop.GET_METHOD)
    def translate_get_method(self, pseudop, args):
        n = args[0]
        i = self.names.index(n)
        yield Opcode.LOAD_METHOD, i


    @translator(Pseudop.CALL_METHOD)
    def translate_call_method(self, pseudop, args):
        yield Opcode.CALL_METHOD, args[0]


    def stack_counter(self, start_size=0):
        return StackCounterCPython37(self, start_size)


class StackCounterCPython37(StackCounterCPython36):


    @stacker(Pseudop.GET_METHOD)
    def stacker_get_method(self, pseudop, args, push, pop):
        pop()    # obj
        push(2)  # obj, callable


    @stacker(Pseudop.CALL_METHOD)
    def stacker_call_method(self, pseudop, args, push, pop):
        pop(args[0] + 2)  # argc, obj, callable
        push()            # result


#
# The end.
