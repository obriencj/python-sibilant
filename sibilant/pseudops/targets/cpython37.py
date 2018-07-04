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


from . import Opcode, Pseudop, translator
from .cpython36 import PseudopsCPython36, direct


class PseudopsCPython37(PseudopsCPython36):
    """
    SpecialCodeSpace emitting bytecode compatible with CPython version
    3.7
    """


    _translations_ = {
        Pseudop.CALL_METHOD: direct(Opcode.CALL_METHOD),
    }


    @translator(Pseudop.LOAD_METHOD)
    def translate_load_method(self, pseudop, args):
        n = args[0]
        i = self.names.index(n)
        yield Opcode.LOAD_METHOD, i


#
# The end.
