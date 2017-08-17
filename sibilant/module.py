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


import types

from io import IOBase
from os.path import split

from sibilant.compiler import iter_compile


__all__ = (
    "create_module", "prep_module", "exec_module"
)


def create_module(name, thing, builtins=None, defaults=None, filename=None):
    mod = types.ModuleType(name)

    prep_module(mod, builtins=builtins, defaults=defaults, filename=filename)
    exec_module(mod, thing, filename=filename)

    return mod


def prep_module(module, builtins=None, defaults=None, filename=None):
    glbls = module.__dict__

    if filename:
        module.__file__ = filename
        module.__path__ = split(filename)

    if defaults:
        glbls.update(defaults)

    if builtins is None:
        builtins = __import__("sibilant.builtins").builtins

    glbls["__builtins__"] = builtins

    return None


def exec_module(module, thing, filename=None):

    consumed = []
    glbls = module.__dict__

    for code in iter_compile(thing, glbls, filename=filename):
        eval(code, glbls)
        consumed.append(code)

    return None


# TODO
# def py_compile_module(module, outfile):
#     _code_to_bytecode(module.__code__)


#
# The end.
