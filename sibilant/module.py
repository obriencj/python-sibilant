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

from sibilant.ast import compose_all_from_str, compose_all_from_stream
from sibilant.compiler import compile_from_ast


__all__ = (
    "module",
)


def module(name, thing, builtins=None, defaults=None):

    if isinstance(thing, str):
        thing = compose_all_from_str(thing)
    elif isinstance(thing, IOBase):
        thing = compose_all_from_stream(thing)
    elif isinstance(thing, Node):
        thing = [thing]
    else:
        raise TypeError("Expected thing type of str, IOBase,"
                        " sibilant.ast.Node")

    mod = types.ModuleType(name)
    glbls = mod.__dict__

    if defaults:
        glbls.update(defaults)

    if builtins is None:
        builtins = sibilant.builtins
    glbls["__builtins__"] = builtins

    for astree in thing:
        code = compile_from_ast(astree, glbls)
        eval(code, glbls)

    return mod


#
# The end.
