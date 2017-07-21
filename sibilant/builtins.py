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
builtin definitions for sibilant. These are made available in the
scope of all loaded modules.

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


def setup():
    bootstrap = __import__("sibilant._bootstrap_builtins")._bootstrap_builtins

    try:
        # in the event that sibilant.importlib.install() has been
        # called, this will work without issue
        builtins = __import__("sibilant._builtins")._builtins

    except ImportError:
        import sys
        from .module import create_module
        from pkg_resources import resource_filename

        filename = resource_filename(__name__, "_builtins.lspy")

        with open(filename, "rt") as fs:
            builtins = create_module("sibilant._builtins", fs,
                                     builtins=bootstrap,
                                     filename=filename)

        sys.modules["sibilant._builtins"] = builtins
        sys.modules["sibilant"]._builtins = builtins

    glbls = globals()
    for module in (bootstrap, builtins):
        for key, val in module.__dict__.items():
            if not key.startswith("__"):
                glbls[key] = val


setup()
del setup


#
# The end.
