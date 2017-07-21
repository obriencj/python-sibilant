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
    import sys
    from os.path import join, dirname
    from .module import create_module
    from pkgutil import get_data

    # because the sibilant.importlib functions will attempt to use this
    # module, we can't rely on them to in-turn load us. Thus for just the
    # builtins module, we'll do it manually.

    bootstrap = __import__("sibilant._bootstrap_builtins")._bootstrap_builtins

    src = get_data(__name__, "_builtins.lspy").decode("utf8")
    filename = join(dirname(__file__), "_builtins.lspy")

    # filename = resource_filename(__name__, "_builtins.lspy")
    # with open(filename, "rt") as fs:

    builtins = create_module("sibilant._builtins", src,
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
