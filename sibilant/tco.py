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
sibilant.lib.tco

Ultra Simple Tail-Calls via Trampoline

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


from ._tco import trampoline, tailcall


__all__ = (
    "trampoline", "tailcall", "tailcall_disable", "tailcall_enable",
)


def tailcall_disable(fun):
    """
    Decorator to instruct the tailcall optimization to never tailcall
    bounce the given function.
    """

    fun._tco_enable = False

    return fun


def tailcall_enable(fun):
    """
    Decorator to instruct the tailcall optimization to tailcall bounce
    the given function.
    """

    fun._tco_enable = True

    return fun


#
# The end.
