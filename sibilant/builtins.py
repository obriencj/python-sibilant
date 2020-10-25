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


def __setup__(glbls, clean=True):
    import sys
    from os.path import join, dirname
    from pkgutil import get_data

    from .module import new_module, init_module, load_module
    from .parse import source_str

    # because the sibilant.importlib functions will attempt to use
    # this module, we can't rely on them to in-turn load us. Thus for
    # just the builtins module, we'll do it manually. this is a simple
    # two-step process. First, we import bootstrap, which is written
    # in Python. It contains a good set of baseline definitions, as
    # well as the special form bindings needed for sibilant modules to
    # execute. Then we'll manually load the basics module, which is
    # written in Sibilant, using bootstrap as its __builtins__
    # definition. Finally, we'll merge the two modules together to
    # form the contents of this module.

    # 1. grab the bootstrap definitions
    import sibilant.bootstrap as bootstrap

    # 2. grab the basics definitions
    src = get_data(__name__, "basics.lspy").decode("utf8")
    filename = join(dirname(glbls.get("__file__", "./")), "basics.lspy")
    source_stream = source_str(src, filename=filename)

    basics = new_module("sibilant.basics")
    init_module(basics, source_stream, builtins=bootstrap)
    load_module(basics)

    sys.modules["sibilant"].basics = basics
    sys.modules["sibilant.basics"] = basics

    # these names get copied into builtins, even though they have
    # secretive-looking names.
    SPECIAL_NAMES = (
        "__import__",
        "__format_value__",
        "__build_string__",
        "__tcr_frame_vars__",
    )

    # 3. merge bootstrap and basics together into this module
    for module in (bootstrap, basics):
        for key, val in module.__dict__.items():
            if (not key.startswith("__")) or key in SPECIAL_NAMES:
                glbls[key] = val

    # 4. optionally remove some stray normal-module definitions from
    # globals
    if clean:
        PURGE_NAMES = (
            "__file__", "__builtins__", "__doc__",
            "__setup__",
        )

        for val in PURGE_NAMES:
            if val in glbls:
                del glbls[val]

    return None


__setup__(globals(), clean=True)


#
# The end.
