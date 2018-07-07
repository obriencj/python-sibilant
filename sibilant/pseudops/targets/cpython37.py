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


from . import Opcode, Pseudop, Symbol, translator
from .cpython36 import PseudopsCPython36, direct


class PseudopsCPython37(PseudopsCPython36):
    """
    Pseudops compiler emitting bytecode compatible with CPython
    version 3.7
    """


    _translations_ = {
        Pseudop.CALL_METHOD: direct(Opcode.CALL_METHOD),
    }


    def pseudop_get_method(self, namesym: Symbol):
        self.request_name(namesym)
        return self.pseudop(Pseudop.GET_METHOD, namesym)


    def pseudop_call_method(self, argc: int):
        return self.pseudop(Pseudop.CALL_METHOD, argc)


    @translator(Pseudop.GET_METHOD)
    def translate_get_method(self, pseudop, args):
        n = args[0]
        i = self.names.index(n)
        yield Opcode.LOAD_METHOD, i


    @translator(Pseudop.CALL_METHOD)
    def translate_call_method(self, pseudop, args):
        pass


#
# The end.
