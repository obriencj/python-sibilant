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


from sibilant.pseudops.targets.cpython37 import PseudopsCPython37
from .cpython36 import SibilantCPython36


class SibilantCPython37(PseudopsCPython37, SibilantCPython36):
    """
    Sibilant compiler for CPython 3.7
    """

    # As of first pass, it looks like there shouldn't be bytecode
    # issues between 3.6 and 3.7 so we'll just import and rename the
    # class
    pass


#
# The end.
