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
from os.path import split, getmtime, getsize

from sibilant.compiler import Mode, iter_compile, code_space_for_version
from sibilant.parse import source_str, source_stream


__all__ = (
    "init_module", "load_module", "prep_module", "exec_module",
    "marshal_module", "exec_marshal_module", "compile_to_file",
)


def init_module(name, builtins=None, defaults=None, filename=None):
    mod = types.ModuleType(name)

    prep_module(mod, builtins=builtins,
                defaults=defaults, filename=filename)

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


def load_module(name, thing, builtins=None, defaults=None, filename=None):

    mod = init_module(name, builtins=builtins,
                      defaults=defaults, filename=filename)

    exec_module(mod, thing, filename=filename)

    return mod


def iter_compile_module(module, thing, filename=None):

    glbls = module.__dict__

    if isinstance(thing, str):
        thing = source_str(thing, filename)

    elif isinstance(thing, IOBase):
        thing = source_stream(thing, filename)

    thing.skip_exec()

    return iter_compile(thing, glbls, filename=filename, mode=Mode.MODULE)


def exec_module(module, thing, filename=None):

    glbls = module.__dict__

    for code in iter_compile_module(module, thing, filename):
        eval(code, glbls)

    return None


def exec_marshal_module(glbls, code_objs):
    builtins = __import__("sibilant.builtins").builtins
    glbls["__builtins__"] = builtins

    for code in code_objs:
        eval(code, glbls)

    return None


def marshal_module(module, thing, filename=None, mtime=0, source_size=0):
    from importlib._bootstrap_external import _code_to_bytecode

    glbls = module.__dict__
    collect = []

    for code in iter_compile_module(module, thing, filename):
        eval(code, glbls)
        collect.append(code)

    factory = code_space_for_version()
    codespace = factory(filename=filename, mode=Mode.MODULE)

    # we can't just have the code object in the file, because we
    # actually have multiple code objects -- one for each top-level
    # compiled expression. We also need to swap out builtins, which
    # isn't as easy as just putting it in globals, since python will
    # optimize away the builtins lookup if globals is the same between
    # two frame (ie. it won't check for new __builtins__ since the
    # globals is the same as the parent). So we cheat. We create a
    # stub code object which simply invokes the exec_marshal_module
    # function with the code objects, which are marshalled as a const
    # tuple.

    # we could have made this easier by making a custom loader for
    # .lspyc files -- but I'd prefer to be able to just reuse the
    # existing python loader for .pyc

    with codespace.activate(glbls):
        codespace.pseudop_get_var("__import__")
        codespace.pseudop_const("sibilant.module")
        codespace.pseudop_call(1)
        codespace.pseudop_get_attr("module")
        codespace.pseudop_get_attr("exec_marshal_module")
        codespace.pseudop_get_var("globals")
        codespace.pseudop_call(0)
        codespace.pseudop_const(tuple(collect))
        codespace.pseudop_call(2)
        codespace.pseudop_pop()
        codespace.pseudop_return_none()

        code = codespace.complete()

    return _code_to_bytecode(code, mtime, source_size)


def compile_to_file(name, source_file, dest_file):
    mtime = getmtime(source_file)
    source_size = getsize(source_file)

    mod = init_module(name, filename=source_file)

    with open(source_file, "rt") as fin:
        bytecode = marshal_module(mod, fin, filename=source_file,
                                  mtime=mtime, source_size=source_size)

    with open(dest_file, "wb") as fout:
        fout.write(bytecode)


#
# The end.
