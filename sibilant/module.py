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
sibilant.module

Module lifecycle for sibilant source files. Provides a read, compile,
and eval sequence for creating a module from sibilant source code.

When a module is loaded from a source file, each top-level expression
is read, then compiled, then evaluated in order. This allows
expressions to modify the module state for successive expressions, by
adding reader or compiler macros for example, or by adjusting aspects
of the compiler.

Expressions can be individually compiled to code objects, and those
code objects can be combined into a single module code object to
produce a .pyc file. When loading a precompiled module in this manner
the parse and compile steps are skipped, and the code objects that
represent the top-level expressions are evaluated in order.

author: Christopher O'Brien <obriencj@gmail.com>
license: LGPL v.3
"""


from functools import partial
from os.path import split, getmtime, getsize
from types import ModuleType

from sibilant.compiler import Mode, code_space_for_version
from sibilant.lib import symbol
from sibilant.tco import trampoline, tailcall, tailcall_enable
from sibilant.parse import default_reader, source_open, source_str


__all__ = (
    "new_module", "fake_module_from_env",
    "init_module", "load_module", "iter_load_module", "load_module_1",
    "parse_time", "compile_time", "hook_compile_time",
    "run_time", "partial_run_time",
    "exec_marshal_module", "marshal_wrapper", "compile_to_file",
)


def new_module(name, package_name=None):
    mod = ModuleType(name)
    if package_name:
        mod.__package__ = package_name
    return mod


class FakeModule():
    pass


def fake_module_from_env(env):
    module = FakeModule()
    module.__dict__ = env
    return module


def init_module(module, source_stream,
                builtins=None, filename=None, defaults=None,
                reader=None, compiler=None, compiler_factory=None,
                compiler_factory_params=None,
                evaluator=None):

    if defaults:
        module.__dict__.update(defaults)

    if source_stream and not filename:
        filename = source_stream.filename

    if filename:
        module.__file__ = filename

        modpath = split(filename)
        if modpath[1].startswith("__init__."):
            module.__path__ = modpath[:1]

    module.__stream__ = source_stream

    if builtins is None:
        import sibilant.builtins as builtins
    module.__builtins__ = builtins

    if reader:
        module.__reader__ = reader

    if compiler:
        module.__compiler__ = compiler

    if compiler_factory:
        module.__compiler_factory__ = compiler_factory

    if compiler_factory_params:
        module.__compiler_factory_params__ = compiler_factory_params

    if evaluator:
        module.__evaluator__ = evaluator

    return module


def get_module_reader(module):
    reader = getattr(module, "__reader__", None)

    if reader is None:
        reader = default_reader
        module.__reader__ = reader

    return reader


def get_module_stream(module):
    stream = getattr(module, "__stream__", None)

    if stream is None:
        stream = source_str("", "<empty>")
        module.__stream__ = stream

    return stream


def parse_time(module):
    reader = get_module_reader(module)
    stream = get_module_stream(module)

    return reader.read(stream)


def get_module_compiler_factory_params(module):
    params = getattr(module, "__compiler_factory_params__", None)

    if params is None:
        params = {
            "name": getattr(module, "__name__", None),
            "filename": getattr(module, "__file__", None),
            "mode": Mode.MODULE,
        }
        module.__compiler_factory_params__ = params

    return params


def get_module_compiler_factory(module):
    factory = getattr(module, "__compiler_factory__", None)

    if factory is None:
        factory = code_space_for_version()
        module.__compiler_factory__ = factory

    return factory


def get_module_compiler(module):
    compiler = getattr(module, "__compiler__", None)

    if compiler is None:
        factory = get_module_compiler_factory(module)
        params = get_module_compiler_factory_params(module)
        compiler = factory(**params)
        module.__compiler__ = compiler

    return compiler


def compile_time(module, source_expr):
    """
    Performs compile-time operations on source_expr in a module
    """

    compiler = get_module_compiler(module)
    module_globals = module.__dict__

    with compiler.activate(module_globals):
        compiler.add_expression_with_return(source_expr)
        code_obj = compiler.complete()

    return code_obj


def hook_compile_time(hook_fn, compile_time=compile_time):
    """
    Creates a compile_time wrapper which will call hook_fn with the
    given module, source_expr, and the code_obj result of
    compile_time. Can be used to accumulate code_obj without
    re-implementing the entire parse, compile, run loop of load_module
    """

    def compile_time_with_hook(module, source_expr):
        code_obj = compile_time(module, source_expr)
        hook_fn(module, source_expr, code_obj)
        return code_obj

    return compile_time_with_hook


def get_module_evaluator(module):
    evaluator = getattr(module, "__evaluator__", None)

    if evaluator is None:
        mod_globals = module.__dict__
        teval = trampoline(eval)

        @trampoline
        def evaluator(code):
            return tailcall(teval)(code, mod_globals)

        module.__evaluator__ = evaluator

    return evaluator


@trampoline
def run_time(module, code_obj):
    evaluator = get_module_evaluator(module)
    return tailcall(evaluator)(code_obj)


@tailcall_enable
def partial_run_time(module, code_obj):
    evaluator = get_module_evaluator(module)
    return partial(evaluator, code_obj)


def load_module(module, parse_time=parse_time,
                compile_time=compile_time, run_time=run_time):

    try:
        while True:
            load_module_1(module, parse_time, compile_time, run_time)

    except StopIteration:
        pass


def iter_load_module(module, parse_time=parse_time,
                     compile_time=compile_time, run_time=run_time):

    while True:
        yield load_module_1(module, parse_time, compile_time, run_time)


@trampoline
def load_module_1(module, parse_time=parse_time,
                  compile_time=compile_time, run_time=run_time):

    source_expr = parse_time(module)

    if source_expr is None:
        raise StopIteration

    else:
        code_obj = compile_time(module, source_expr)
        return tailcall(run_time)(module, code_obj)


def exec_marshal_module(glbls, code_objs, builtins=None):
    """
    Invoked during loading of modules expored via marshal_wrapper
    """

    mod = fake_module_from_env(glbls)
    init_module(mod, None, builtins=builtins)

    # skips the read time and compile time stages of import, just
    # performs run time for every compiled expression to set up the
    # module

    for code_obj in code_objs:
        run_time(mod, code_obj)

    return None


def marshal_wrapper(code_objs, filename=None, mtime=0, source_size=0,
                    builtins_name=None):

    from importlib._bootstrap_external import _code_to_bytecode

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

    with codespace.activate({}):

        # import and obtain sibilant.module.exec_marshal_module
        codespace.pseudop_const(0)
        codespace.pseudop_const("exec_marshal_module")
        codespace.pseudop_build_tuple(1)
        codespace.pseudop_import_name(symbol("sibilant.module"))
        codespace.pseudop_import_from(symbol("exec_marshal_module"))
        codespace.pseudop_rot_two()
        codespace.pseudop_pop()

        # argument 1. globals()
        codespace.pseudop_get_var(symbol("globals"))
        codespace.pseudop_call(0)

        # argument 2. tuple(code_objs)
        codespace.pseudop_const(tuple(code_objs))

        # argument 3. builtins (either specified or None)
        if builtins_name:
            codespace.pseudop_const(0)
            codespace.pseudop_const("nil")
            codespace.pseudop_build_tuple(1)
            codespace.pseudop_import_name(builtins_name)
        else:
            codespace.pseudop_const(None)

        codespace.pseudop_call(3)
        codespace.pseudop_return()

        code = codespace.complete()

    return _code_to_bytecode(code, mtime, source_size)


def compile_to_file(name, pkgname, source_file, dest_file,
                    builtins_name=None):

    mtime = getmtime(source_file)
    source_size = getsize(source_file)

    code_objs = []

    def accumulate(_mod, _sexpr, code):
        code_objs.append(code)

    with source_open(source_file) as source_stream:
        mod = new_module(name, package_name=pkgname)
        init_module(mod, source_stream)
        load_module(mod, compile_time=hook_compile_time(accumulate))

    bytecode = marshal_wrapper(code_objs, filename=source_file,
                               mtime=mtime, source_size=source_size,
                               builtins_name=builtins_name)

    with open(dest_file, "wb") as dest_stream:
        dest_stream.write(bytecode)


# ;; async variations

async def async_parse_time(module):
    reader = get_module_reader(module)
    stream = get_module_stream(module)

    print(" == async_parse_time ==", "calling reader.read")
    r = reader.read(stream)
    print(" == async_parse_time ==", r)
    return r


async def async_load_module_1(module, async_parse_time=async_parse_time,
                              compile_time=compile_time, run_time=run_time):

    print(" == async_load_module_1 ==", "await on async_parse_time")
    source_expr = await async_parse_time(module)
    print(" == async_load_module_1 ==", source_expr)

    if source_expr is None:
        raise StopIteration

    else:
        code_obj = compile_time(module, source_expr)
        return run_time(module, code_obj)


#
# The end.
