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
Sibilant, a LISP for Python

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


def __auto_enable_importer():
    from os import environ
    from sys import _xoptions as xoption

    xopt = xoption.get("SIBILANT_NOIMPORTER", "0") == "0"
    eopt = environ.get("SIBILANT_NOIMPORTER", "0") == "0"

    if xopt and eopt:
        from .importlib import install
        install()
        return True
    else:
        return False


__auto_enable_importer()


#
# The end.
