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


from functools import partial
from os.path import split, getmtime, getsize
from types import ModuleType

from sibilant.compiler import Mode, code_space_for_version
from sibilant.parse import default_reader, source_open, source_str


__all__ = (
    "new_module", "fake_module_from_env",
    "init_module", "load_module", "iter_load_module", "load_module_1",
    "parse_time", "compile_time", "hook_compile_time",
    "run_time", "partial_run_time",
    "exec_marshal_module", "marshal_wrapper", "compile_to_file",
)


def new_module(name):
    return ModuleType(name)


class FakeModule():
    pass


def fake_module_from_env(env):
    module = FakeModule()
    module.__dict__ = env
    return module


def init_module(module, source_stream, builtins,
                filename=None, defaults=None,
                reader=None, compiler=None, compiler_factory=None,
                compiler_factory_params=None,
                evaluator=None):

    if defaults:
        module.__dict__.update(defaults)

    if filename:
        module.__file__ = filename
        module.__path__ = split(filename)

    module.__stream__ = source_stream

    if builtins is None:
        builtins = __import__("sibilant.builtins").builtins
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
    try:
        reader = module.__reader__
    except:
        reader = default_reader
        module.__reader__ = reader

    return reader


def get_module_stream(module):
    try:
        stream = module.__stream__
    except:
        stream = source_str("", "<empty>")
        module.__stream__ = stream

    return stream


def parse_time(module):
    reader = get_module_reader(module)
    stream = get_module_stream(module)

    return reader.read(stream)


def get_module_compiler_factory_params(module):
    try:
        params = module.__compiler_factory_params__

    except:
        params = {
            "name": getattr(module, "__name__", None),
            "filename": getattr(module, "__file__", None),
            "mode": Mode.MODULE,
        }
        module.__compiler_factory_params__ = params

    return params


def get_module_compiler_factory(module):
    try:
        factory = module.__compiler_factory__

    except:
        factory = code_space_for_version()
        module.__compiler_factory__ = factory

    return factory


def get_module_compiler(module):
    try:
        compiler = module.__compiler__

    except:
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
    try:
        evaluator = module.__evaluator__

    except:
        mod_globals = module.__dict__

        def evaluator(code):
            return eval(code, mod_globals)

        module.__evaluator__ = evaluator

    return evaluator


def run_time(module, code_obj):
    evaluator = get_module_evaluator(module)
    return evaluator(code_obj)


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


def load_module_1(module, parse_time=parse_time,
                  compile_time=compile_time, run_time=run_time):

    source_expr = parse_time(module)

    if source_expr is None:
        raise StopIteration

    else:
        code_obj = compile_time(module, source_expr)
        return run_time(module, code_obj)


def exec_marshal_module(glbls, code_objs):
    """
    Invoked during loading of modules expored via marshal_wrapper
    """

    mod = fake_module_from_env(glbls)
    init_module(mod, None, None)

    # skips the read time and compile time stages of import, just
    # performs run time for every compiled expression to set up the
    # module

    for code_obj in code_objs:
        run_time(mod, code_obj)

    return None


def marshal_wrapper(code_objs, filename=None, mtime=0, source_size=0):
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
        codespace.pseudop_get_var("__import__")
        codespace.pseudop_const("sibilant.module")
        codespace.pseudop_call(1)
        codespace.pseudop_get_attr("module")
        codespace.pseudop_get_attr("exec_marshal_module")
        codespace.pseudop_get_var("globals")
        codespace.pseudop_call(0)
        codespace.pseudop_const(tuple(code_objs))
        codespace.pseudop_call(2)
        codespace.pseudop_return()

        code = codespace.complete()

    return _code_to_bytecode(code, mtime, source_size)


def compile_to_file(name, source_file, dest_file):
    mtime = getmtime(source_file)
    source_size = getsize(source_file)

    code_objs = []

    def accumulate(_mod, _sexpr, code):
        code_objs.append(code)

    with source_open(source_file) as source_stream:
        mod = new_module(name)
        init_module(mod, source_stream, None, filename=source_file)
        load_module(mod, compile_time=hook_compile_time(accumulate))

    bytecode = marshal_wrapper(code_objs, filename=source_file,
                               mtime=mtime, source_size=source_size)

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
