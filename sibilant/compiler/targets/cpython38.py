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


from sibilant.pseudops.targets.cpython38 import PseudopsCPython38
from .cpython37 import SibilantCPython37


class SibilantCPython38(PseudopsCPython38, SibilantCPython37):
    """
    Sibilant compiler for CPython 3.8
    """

    # As of first pass, it looks like there shouldn't be bytecode
    # issues between 3.7 and 3.8 so we'll just import and rename the
    # class
    pass


#
# The end.
