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
Sibilant Example Project

:author: Christopher O'Brien  <obriencj@gmail.com>
:license: LGPL v.3
"""


# this is the only part that is really important. You need to have
# your top-level package or module enable the sibilant importer. The
# easiest way to do that is to just import sibilant
import sibilant  # noqa


from functools import lru_cache


# it's up to you if you want to bother including anything else in
# your top-level.

def some_kind_of_function():
    return "whatever"


@lru_cache(10000)
def pyfib_lru(index):
    if index < 2:
        return index
    else:
        return pyfib_lru(index - 1) + pyfib_lru(index - 2)


def pyfibonacci_lru(index):
    pyfib_lru.cache_clear()
    return pyfib_lru(index)


def pyfibonacci_notc(index, carry=0, accu=1):
    if index == 0:
        result = carry
    else:
        result = pyfibonacci_notc(index - 1, accu, accu + carry)
    return result


def pyfibonacci_while(index):
    carry = 0
    accu = 1

    while index > 0:
        index, carry, accu = index - 1, accu, accu + carry

    return carry


#
# The end.
