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


__all__ = ( "ModuleType", "module", "module_from_parser",
            "module_from_stream", "module_from_str" )


class ModuleType(types.ModuleType):
    pass


def module(name, sib_ast, positions=None, builtins=None, defaults=None):
    mod = ModuleType(name)

    if defaults:
        mod.__dict__.update(defaults)

    positions = positions or dict()
    evaluate(sib_ast, positions, module)
    return module


def module_from_parser(name, parser_gen):
    return module(name, compose(parser_gen))


def module_from_stream(name, stream):
    return module(name, compose_from_stream(stream))


def module_from_str(name, src_str):
    return module(name, compose_from_str(src_str))


#
# The end.
