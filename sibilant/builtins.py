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
sibilant.builtins

builtin definitions for sibilant. These are made available in the
scope of all loaded modules. This is a combination of the
sibilant.bootstrap and sibilant.basics modules

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


def __setup__(glbls):
    import sys
    from os.path import join, dirname
    from pkgutil import get_data

    from .module import new_module, init_module, load_module
    from .parse import source_str

    # because the sibilant.importlib functions will attempt to use
    # this module, we can't rely on them to in-turn load us. Thus for
    # just the builtins module, we'll do it manually. this is a simple
    # two-step process. First, we import the _bootstrap_builtins,
    # which is written in Python, and contains a good set of baseline
    # definitions, as well as the special form bindings needed for
    # sibilant modules to execute. then we'll load the _builtins
    # sibilant module with the bootstrap as its __builtins__
    # definition. Finally, we'll merge the two modules together to
    # form the contents of this module.

    bootstrap = __import__("sibilant.bootstrap").bootstrap

    src = get_data(__name__, "basics.lspy").decode("utf8")
    filename = join(dirname(__file__), "basics.lspy")

    basics = new_module("sibilant.basics")
    source_stream = source_str(src, filename=filename)
    init_module(basics, source_stream, bootstrap, filename=filename)
    load_module(basics)

    sys.modules["sibilant"].basics = basics
    sys.modules["sibilant.basics"] = basics

    _all = set()
    for module in (bootstrap, basics):
        for key, val in module.__dict__.items():
            if not key.startswith("__"):
                glbls[key] = val
                _all.add(key)

    return tuple(_all)


__all__ = __setup__(globals())
del __setup__


#
# The end.
