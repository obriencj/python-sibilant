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


import sys

from collections import MutableMapping
from functools import partial
from os.path import split, getmtime, getsize
from types import ModuleType

from sibilant.compiler import Mode, compiler_for_version
from sibilant.lib import symbol, trampoline, tailcall, tailcall_enable
from sibilant.parse import default_reader, source_open, source_str


__all__ = (
    "new_module", "fake_module_from_env",
    "init_module", "load_module", "iter_load_module", "load_module_1",
    "parse_time", "compile_time", "hook_compile_time",
    "run_time", "partial_run_time",
    "exec_marshal_module", "marshal_wrapper", "compile_to_file",
)


def new_module(name, package_name=None, system=True):
    mod = ModuleType(name)
    if package_name:
        mod.__package__ = package_name

    if system:
        sys.modules[package_name or name] = mod

    return mod


class FakeModule(object):
    """
    A simple object wrapper for the given dictionary environment.
    """

    # note that this cannot inherit from ModuleType, as that does not
    # allow instances to replace the __dict__ attribute. We don't want
    # to make a copy, or back-fill the existing __dict__ either, since
    # we really just want to be a wrapper.

    def __init__(self, env=None):
        if env is None:
            # whatever, we've already had an instance allocated with its own
            # dict, just use that.
            return
        elif isinstance(env, MutableMapping):
            # cool, use this environment instead of the one we were
            # allocated with.
            self.__dict__ = env
        else:
            # this might fail in instances where we cannot snag an
            # object's underlying vars, but we'll give it a try at
            # least.
            self.__dict__ = vars(env)


def fake_module_from_env(env):
    """
    Produce an object wrapper for the given environment
    """

    return FakeModule(env)


def init_module(module, source_stream,
                builtins=None, filename=None, defaults=None,
                reader=None, compiler=None, compiler_factory=None,
                compiler_factory_params=None,
                evaluator=None):

    """
    Initializes a module object for use with sibilant.

    The module can either be an object, or a MutableMapping instance
    such as a dict. If it is a mapping, then a new FakeModule will be
    creating to act as an object wrapper for the mapping.

    Returns the module object or the FakeModule that was created.
    """

    # while FakeModule can wrap another object as needed, we want to
    # preserve the object we're passed in this case, and so only wrap
    # MutableMappings.
    if isinstance(module, MutableMapping):
        module = fake_module_from_env(module)

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


def set_module_reader(module, reader):
    module.__reader__ = reader


def get_module_stream(module):
    stream = getattr(module, "__stream__", None)

    if stream is None:
        stream = source_str("", "<empty>")
        module.__stream__ = stream

    return stream


def set_module_stream(module, stream):
    module.__stream__ = stream


def parse_time(module):
    """
    Enter Parse-Time on the module, producing a single top-level
    source expression using the modules's reader and source stream.
    """

    reader = get_module_reader(module)
    stream = get_module_stream(module)

    return reader.read(stream)


def get_module_compiler_factory_params(module):
    """
    Get the compiler factory params from the module. If the global
    variable __compiler_factory_params__ is not set, assign and return
    the default parameters dict.
    """

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
    """
    Get the compiler factory for the given module. This is the
    function which will be invoked with the compiler factory params to
    produce the default module compiler instance.
    """

    factory = getattr(module, "__compiler_factory__", None)

    if factory is None:
        factory = compiler_for_version()
        module.__compiler_factory__ = factory

    return factory


def set_module_compiler_factory(module, factory):
    module.__compiler_factory__ = factory


def get_module_compiler(module):
    """
    Get the compiler instance for the given module. If it has not been
    assigned to the __compiler__ global variable, produce a new
    compiler instance, assign it, and return it.
    """

    compiler = getattr(module, "__compiler__", None)

    if compiler is None:
        factory = get_module_compiler_factory(module)
        params = get_module_compiler_factory_params(module)
        compiler = factory(**params)
        module.__compiler__ = compiler

    return compiler


def set_module_compiler(module, compiler):
    module.__compiler__ = compiler


def compile_time(module, source_expr):
    """
    Performs Compile-Time on a source expression, using the module's
    compiler. Produces the compiled result.
    """

    compiler = get_module_compiler(module)

    with compiler.active_context(module):
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
    """
    Given a module, find its evaluator function. If one hasn't been
    assigned to the global __evaluator__ variable, then use a
    trampoline-wrapped eval as a default, and assign it as
    __evaluator__ for use next time.
    """

    evaluator = getattr(module, "__evaluator__", None)

    if evaluator is None:
        mod_globals = module.__dict__
        teval = trampoline(eval)

        @trampoline
        def evaluator(code):
            return tailcall(teval)(code, mod_globals)

        module.__evaluator__ = evaluator

    return evaluator


def set_module_evaluator(module, evaluator):
    module.__evaluator__ = evaluator


@trampoline
def run_time(module, code_obj):
    """
    The Run-Time phase for a module. Evaluates the recently compiled
    code_obj in the module's environment using the module's evaluator,
    and produces the expression's resulting value.
    """

    evaluator = get_module_evaluator(module)
    return tailcall(evaluator)(code_obj)


@tailcall_enable
def partial_run_time(module, code_obj):
    """
    Produces a callable which when called will execute the Run-Time
    phase for the given module for the the given compiled code_obj.
    """

    evaluator = get_module_evaluator(module)
    return partial(evaluator, code_obj)


def load_module(module, parse_time=parse_time,
                compile_time=compile_time, run_time=run_time):

    """
    Parse, compile, and evaluate all of the expressions in a module.
    """

    while True:
        source_expr = parse_time(module)
        if source_expr is None:
            break

        code_obj = compile_time(module, source_expr)
        run_time(module, code_obj)


def iter_load_module(module, parse_time=parse_time,
                     compile_time=compile_time, run_time=run_time):

    """
    Iterator which reads a single expression from a module's source
    stream, compile it, evaluate it, yields the result, then repeats
    until the source stream is empty.
    """

    while True:
        source_expr = parse_time(module)
        if source_expr is None:
            break

        code_obj = compile_time(module, source_expr)
        yield run_time(module, code_obj)


@trampoline
def load_module_1(module, parse_time=parse_time,
                  compile_time=compile_time, run_time=run_time):

    """
    Parse a single expression from a module's source stream, compile
    it, evaluate it, and return the result.
    """

    source_expr = parse_time(module)
    if source_expr is None:
        return None

    code_obj = compile_time(module, source_expr)
    return tailcall(run_time)(module, code_obj)


def exec_marshal_module(glbls, code_objs, builtins=None):
    """
    Invoked during loading of modules expored via marshal_wrapper
    """

    # mod = fake_module_from_env(glbls)
    mod = init_module(glbls, None, builtins=builtins)

    # skips the read time and compile time stages of import, just
    # performs run time for every compiled expression to set up the
    # module

    for code_obj in code_objs:
        run_time(mod, code_obj)

    return None


def marshal_wrapper(code_objs, filename=None, mtime=0, source_size=0,
                    builtins_name=None):

    """
    Produce a collection of bytes representing the compiled form of a
    series of statements (as compiled code objects).
    """

    import importlib._bootstrap_external as ibe
    try:
        pyc = ibe._code_to_bytecode
    except AttributeError:
        pyc = ibe._code_to_timestamp_pyc

    factory = compiler_for_version()
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

    with codespace.active_context({}):

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

    return pyc(code, mtime, source_size)


def compile_to_file(name, pkgname, source_file, dest_file,
                    builtins_name=None):

    """
    Produce a python compiled bytecode file from a sibilant source
    code file.
    """

    mtime = getmtime(source_file)
    source_size = getsize(source_file)

    code_objs = []

    def accumulate(_mod, _sexpr, code):
        code_objs.append(code)

    with source_open(source_file) as source_stream:
        mod = new_module(name, package_name=pkgname)
        mod = init_module(mod, source_stream)
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
