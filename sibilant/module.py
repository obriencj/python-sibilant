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
import sibilant.builtins

from io import IOBase

from sibilant.ast import Node, compose_all_from_str, compose_all_from_stream
from sibilant.compiler import compile_from_ast


__all__ = (
    "create_module", "prep_module", "exec_module"
)


def create_module(name, thing, builtins=None, defaults=None):

    mod = types.ModuleType(name)

    prep_module(mod, builtins=builtins, defaults=defaults)
    exec_module(mod, thing)

    return mod


def prep_module(module, builtins=None, defaults=None):
    glbls = module.__dict__

    if defaults:
        glbls.update(defaults)

    if builtins is None:
        builtins = sibilant.builtins
    glbls["__builtins__"] = builtins


def exec_module(module, thing, filename=None):

    if isinstance(thing, str):
        thing = tuple(compose_all_from_str(thing))
    elif isinstance(thing, IOBase):
        thing = tuple(compose_all_from_stream(thing))
    elif isinstance(thing, Node):
        thing = (thing,)
    else:
        raise TypeError("Expected thing type of str, IOBase,"
                        " sibilant.ast.Node, not %r" % type(thing))

    consumed = []
    glbls = module.__dict__

    for astree in thing:
        code = compile_from_ast(astree, glbls, filename=filename)
        eval(code, glbls)
        consumed.append(code)

    # TODO
    # module.__code__ = merge_code(consumed)


# TODO
# def py_compile_module(module, outfile):
#     _code_to_bytecode(module.__code__)


#
# The end.
