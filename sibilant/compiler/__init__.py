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
sibilant.compiler

The sibilant compiler. Converts expressions into Python bytecode.

author: Christopher O'Brien <obriencj@gmail.com>
license: LGPL v.3
"""


import threading

from abc import ABCMeta, abstractmethod
from collections import Mapping
from contextlib import contextmanager
from functools import partial
from itertools import count
from os.path import exists
from platform import python_implementation
from sys import version_info
from typing import Union

from sibilant.lib import (
    SibilantException, SibilantSyntaxError,
    symbol, is_symbol,
    lazygensym, is_lazygensym,
    keyword, is_keyword,
    pair, cons, is_pair, is_proper, nil, is_nil,
    get_position, fill_position,
)

from sibilant.pseudops import (
    PseudopsCompiler, Mode,
    CONST_TYPES, Constant,
)

from sibilant.tco import trampoline, tailcall


__all__ = (
    "UnsupportedVersion",
    "CompilerSyntaxError",
    "Mode",
    "SibilantCompiler",
    "compiler_for_version",
    "Compiled", "is_compiled",
    "Special", "is_special",
    "Macro", "is_macro",
    "Alias", "is_alias",
    "Operator", "is_operator",
    "gather_formals", "gather_parameters",
    "env_find_compiled", "env_get_expander",
)


_keyword_star = keyword("*")
_keyword_starstar = keyword("**")

_symbol_attr = symbol("attr")
_symbol_nil = symbol("nil")
_symbol_None = symbol("None")
_symbol_True = symbol("True")
_symbol_False = symbol("False")
_symbol_ellipsis = symbol("...")
_symbol_keyword = symbol("keyword")
_symbol_tailcall = symbol("tailcall")


Symbol = Union[lazygensym, symbol]

COMPILER_DEBUG = False

# this is an amount to pad out all max_stack allocations
STACK_SAFETY = 2

_active = threading.local()


def current():
    try:
        return _active.compiler
    except AttributeError:
        _active.compiler = None
        return None


def set_current(compiler):
    _active.compiler = compiler


class CompilerException(Exception):
    pass


class CompilerSyntaxError(SibilantSyntaxError):
    pass


class UnsupportedVersion(SibilantException):
    pass


class Compiled(metaclass=ABCMeta):
    __objname__ = "sibilant compiled"


    def __init__(self, name):
        self.__name__ = name


    def __call__(self, *args, **kwds):
        msg = "Attempt to call %r as a runtime function" % self
        raise TypeError(msg)


    def __repr__(self):
        return "<%s %r>" % (self.__objname__, self.__name__)


    @abstractmethod
    def compile(self, compiler, source_obj, tc, cont):
        pass


def is_compiled(obj):
    return isinstance(obj, Compiled)


class Special(Compiled):
    __objname__ = "special form"


    def __init__(self, name, compilefn):
        super().__init__(name)


    def __new__(cls, name, compilefn):
        if not callable(compilefn):
            msg = "compilefn must be callable, not %r" % compilefn
            raise SibilantException(msg)

        nom = str(name or compilefn.__name__)
        mbs = {
            "__doc__": compilefn.__doc__,
            "compile_impl": staticmethod(compilefn),
        }
        cls = type(nom, (cls, ), mbs)
        return object.__new__(cls)


    @trampoline
    def compile(self, compiler, source_obj, tc, cont):
        result = self.compile_impl(compiler, source_obj, tc)
        return tailcall(cont)(result, tc)


def is_special(obj):
    return isinstance(obj, Special)


class Macro(Compiled):
    """
    A Macro is defined at run-time but consumed at compile-time to
    transform a source expression. It is an error to invoke it as
    a callable at run-time.

    Call the `expand` method with a full source expression to obtain
    the one-time transformed result.
    """

    __objname__ = "macro"


    def __init__(self, name, macrofn):
        super().__init__(name)
        self._proper = True


    def __new__(cls, name, expandfn):
        if not callable(expandfn):
            msg = "expandfn must be callable, not %r" % expandfn
            raise SibilantException(msg)

        nom = str(name or expandfn.__name__)
        mbs = {
            "__doc__": expandfn.__doc__,
            "expand": staticmethod(expandfn),
        }
        cls = type(nom, (cls, ), mbs)
        return object.__new__(cls)


    @trampoline
    def compile(self, compiler, source_obj, tc, cont):
        called_by, source = source_obj

        if self._proper:
            position = source_obj.get_position()
            args, kwargs = simple_parameters(source, position)
            expr = self.expand(*args, **kwargs)

        else:
            expr = self.expand(*source.unpack())

        expr = _symbol_None if expr is None else expr

        fill_position(expr, source_obj.get_position())
        return tailcall(cont)(expr, tc)


def is_macro(obj):
    return isinstance(obj, Macro)


class Alias(Macro):
    __objname__ = "alias"


    def compile(self, compiler, source_obj, tc, cont):
        expanded = self.expand()
        expanded = _symbol_None if expanded is None else expanded

        if is_pair(source_obj):
            called_by, source = source_obj
            res = cons(expanded, source)
            fill_position(res, source_obj.get_position())
            expanded = res

        return tailcall(cont)(expanded, tc)


def is_alias(obj):
    return isinstance(obj, Alias)


class Syntax(Compiled):
    """
    A transformation which operates directly on the source expression
    and returns a transformed one. Similar to a macro, but invokes its
    processing function without any argument unpacking.
    """

    __objname__ = "syntax"


    def __init__(self, name, macrofn):
        super().__init__(name)


    def __new__(cls, name, expandfn):
        if not callable(expandfn):
            msg = "expandfn must be callable, not %r" % expandfn
            raise SibilantException(msg)

        nom = str(name or expandfn.__name__)
        mbs = {
            "__doc__": expandfn.__doc__,
            "transform": staticmethod(expandfn),
        }
        cls = type(nom, (cls, ), mbs)
        return object.__new__(cls)


    def compile(self, compiler, source_obj, tc=False):
        res = self.transform(source_obj)
        return _symbol_None if res is None else res


def is_syntax(obj):
    return isinstance(obj, Syntax)


class Operator(Compiled):
    __objname__ = "operator"


    def __init__(self, name, compilefn, runtimefn):
        super().__init__(name)


    def __new__(cls, name, compilefn, runtimefn):
        assert(compilefn is not None)
        assert(runtimefn is not None)

        if not callable(compilefn):
            msg = "compilefn must be callable, not %r" % compilefn
            raise SibilantException(msg)

        if not callable(runtimefn):
            msg = "runtimefn must be callable, not %r" % runtimefn
            raise SibilantException(msg)

        nom = str(name or runtimefn.__name__ or compilefn.__name__)
        mbs = {
            "__doc__": compilefn.__doc__,
            "__call__": staticmethod(runtimefn),
            "compile_impl": staticmethod(compilefn),
        }
        cls = type(nom, (cls, ), mbs)
        return object.__new__(cls)


    @trampoline
    def compile(self, compiler, source_obj, tc, cont):
        result = self.compile_impl(compiler, source_obj, tc)
        return tailcall(cont)(result, tc)


def is_operator(obj):
    return isinstance(obj, Operator)


def _label_generator(formatstr="label_{:04x}"):
    return partial(next, map(formatstr.format, count()))


def compiler_for_version(ver=version_info,
                         impl=python_implementation()):
    """
    Returns the relevant SibilantCompiler subclass to emit bytecode
    for the relevant version and implementation of Python. Raises
    UnsupportedVersion if the version and/or implementation aren't
    supported.
    """

    if impl == 'CPython':
        # TODO : user some sort of introspection instead of having to
        # write an import for every case...

        if (3, 7) <= ver <= (3, 8):
            from .targets.cpython37 import SibilantCPython37
            return SibilantCPython37

        elif (3, 6) <= ver <= (3, 7):
            from .targets.cpython36 import SibilantCPython36
            return SibilantCPython36

        elif (3, 5) <= ver <= (3, 6):
            from .targets.cpython35 import SibilantCPython35
            return SibilantCPython35

    raise UnsupportedVersion(ver, impl)


class SibilantCompiler(PseudopsCompiler, metaclass=ABCMeta):


    def __init__(self, tco_enabled=True, self_ref=None, **kwopts):

        # TODO: using **kwops is crap, maybe we need a copiler options
        # object to document the options and what they mean.

        super().__init__(**kwopts)

        self.env = None
        self.env_tmp_compiled = []

        self.tco_enabled = tco_enabled
        self.tailcalls = 0

        self.self_ref = self_ref
        if self_ref:
            self.request_var(self_ref)


    def __del__(self):
        super().__del__()
        del self.env
        del self.env_tmp_compiled


    def activate(self, environment):
        """
        Associates the compiler with an environment. The environment
        will provide references to special forms and macros that are
        available to the compiler.
        """

        if not isinstance(environment, Mapping):
            environment = vars(environment)
        self.env = environment


    def require_active(self):
        if self.env is None:
            raise CompilerException("compiler code space is not active")


    def reset(self):
        super().reset()
        self.tailcalls = 0
        self.self_ref = None
        self.env = None


    @contextmanager
    def active_context(self, env):
        """
        Binds to an environment, clears self at end of context
        """

        self.activate(env)

        old_compiler = current()
        set_current(self)

        try:
            yield self

        finally:
            set_current(old_compiler)
            self.reset()


    def child(self, **addtl):
        addtl.setdefault("tco_enabled", self.tco_enabled)
        return super().child(**addtl)


    def child_context(self, **kwargs):
        """
        Returns an active context for a child codespace
        """

        self.require_active()
        cs = self.child(**kwargs)
        return cs.active_context(self.env)


    @trampoline
    def compile(self, source_obj, tc, cont):
        """
        Compile a supported source object into an expression.

        pair, symbol, keyword, and the pythonic constant types are
        valid source obj types.
        """

        tc = self.tco_enabled and tc

        if is_pair(source_obj):
            dispatch = self.compile_pair
        elif is_symbol(source_obj) or is_lazygensym(source_obj):
            dispatch = self.compile_symbol
        elif is_keyword(source_obj):
            dispatch = self.compile_keyword
        elif isinstance(source_obj, CONST_TYPES):
            dispatch = self.compile_constant
        else:
            msg = "Unsupported source object %r" % source_obj
            raise self.error(msg, source_obj)

        return tailcall(dispatch)(source_obj, tc, cont or self._compile_cont)


    @trampoline
    def _compile_cont(self, source_obj, tc):
        """
        The default continuation for compile. If the result was a new
        source object (anything other than None), will restart the
        compile.
        """

        if source_obj is None:
            # None explicitly means that the compilation resulted in
            # no new forms, so we're done.
            return None
        else:
            # anything else is a transformation, and needs to be
            # compiled.
            return tailcall(self.compile)(source_obj, tc, None)


    @trampoline
    def compile_pair(self, source_obj: pair, tc, cont):
        """
        Compile a pair expression. This will become either a literal nil,
        a macro expansion, a special invocation, or a runtime function
        application.
        """

        if is_nil(source_obj):
            return tailcall(self.compile_nil)(source_obj, tc, cont)

        if not is_proper(source_obj):
            msg = "cannot evaluate improper lists as expressions"
            raise self._err(msg, source_obj)

        self.pseudop_position_of(source_obj)

        head, tail = source_obj

        if is_symbol(head) or is_lazygensym(head):
            comp = self.find_compiled(head)
            if comp:
                # the head of the pair is a symbolic reference which
                # resolved to a compile-time object. Invoke that.
                return tailcall(comp.compile)(self, source_obj, tc, cont)

            else:
                return tailcall(self.compile_apply)(source_obj, tc, cont)

        elif is_pair(head):
            return tailcall(self.compile_apply)(source_obj, tc, cont)

        else:
            # TODO: should this be a compile-time error? If we have
            # something that isn't a symbolic reference or isn't a
            # pair, then WTF else would it be? Let's just let it break
            # at runtime, for now.
            return tailcall(self.compile_apply)(source_obj, tc, cont)


    @trampoline
    def compile_apply(self, source_obj: pair, tc, cont):
        """
        Compile a runtime function apply expression.
        """

        head, tail = source_obj

        pos = source_obj.get_position()

        if is_pair(head):
            @trampoline
            def ccp(new_head, tc):
                # Continue Compiling Pair. This is how we finish
                # compiling a function invocation after first
                # compiling the head

                if new_head is None:
                    # the original head pair compiled down to a None,
                    # which means it pushed bytecode and left a value
                    # on the stack. Complete the apply based on that.
                    return tailcall(self.complete_apply)(tail, pos, tc, cont)
                else:
                    # the original head pair was transformed, so now
                    # we need to start over in a new compile_pair call
                    # using a newly assembled expression.
                    expr = pair(new_head, tail)
                    expr.set_position(pos)
                    return tailcall(self.compile_pair)(expr, tc, cont)

            # we need to compile the head first, to figure out if it
            # expands into a symbolic reference or something. We'll
            # use ccp as a temporary continuation. Note that the
            # evaluation of the head of the pair is never a tailcall
            # itself, even if it would be a tailcall to apply it as a
            # function afterwards.
            return tailcall(self.compile_pair)(head, False, ccp)

        else:
            self.add_expression(head)
            return tailcall(self.complete_apply)(tail, pos, tc, cont)


    @abstractmethod
    def complete_apply(self, arg_source: pair, position, tc, cont):
        """
        This abstract method must be implemented in the version-specific
        python target. It is presumed that the function to apply is
        already evaluated and at TOS. The arg_source will be the
        remainder of the apply form (a cons list of the positional and
        keyword arguments, if any). It is up to the target to compile
        the argument expressions and collect them into the appropriate
        version-specific call opcodes. It is also up to the target to
        determine whether or not to invoke the helper_tailcall_tos in
        order to convert the TOS value into a tailcall.
        """

        pass


    @trampoline
    def compile_symbol(self, sym: Symbol, tc, cont):
        """
        Compile a symbol expression. This can result in a constant for
        certain specialty Python values (None, True, False, and ...)

        Dotted symbols will be compiled into attr calls. Non-dotted
        symbols will be compiled into variable references.

        If a symbol correlates to an Alias in the module namespace,
        then that alias will be expanded and compilation will continue
        from the expanded form.
        """

        comp = self.find_compiled(sym)
        if comp and is_alias(comp):
            return tailcall(comp.compile)(self, sym, tc, cont)

        elif sym is _symbol_None:
            return tailcall(self.compile_constant)(None, tc, cont)

        elif sym is _symbol_True:
            return tailcall(self.compile_constant)(True, tc, cont)

        elif sym is _symbol_False:
            return tailcall(self.compile_constant)(False, tc, cont)

        elif sym is _symbol_ellipsis:
            return tailcall(self.compile_constant)(..., tc, cont)

        elif is_lazygensym(sym):
            return tailcall(cont)(self.pseudop_get_var(sym), tc)

        else:
            ex = sym.rsplit(".", 1)
            if len(ex) == 1:
                return tailcall(cont)(self.pseudop_get_var(sym), None)
            else:
                source = cons(_symbol_attr, *ex, nil)
                return tailcall(self.compile)(source, tc, cont)


    @trampoline
    def compile_keyword(self, kwd: keyword, tc, cont):
        """
        Compile a keyword expression
        """

        source = cons(_symbol_keyword, str(kwd), nil)
        return tailcall(self.compile)(source, False, cont)


    @trampoline
    def compile_constant(self, value: Constant, tc, cont):
        """
        Compile a constant value expression
        """

        return tailcall(cont)(self.pseudop_const(value), tc)


    @trampoline
    def compile_nil(self, nilv: nil, tc, cont):
        """
        Compile a literal nil expression
        """

        return tailcall(cont)(self.pseudop_get_global(_symbol_nil), tc)


    def helper_tailcall_tos(self, args, position):
        """
        Wraps a function invocation into a tailcall. This should only be
        invoked if it has already been determined that a tail-call
        function apply is happening, and the function object is at TOS.
        """

        self.declare_tailcall()

        self.helper_tailrecur_tos(args, position)

        self.pseudop_get_global(_symbol_tailcall)
        self.pseudop_rot_two()
        self.pseudop_call(1)


    def helper_tailrecur_tos(self, args, position):
        """
        Checks for a tail recursion invocation. This should only be
        invoked if it has already been determined that a tail-call
        function apply is happening, and the function object is at
        TOS. This will see if it's possible to convert the apply into
        calls to assign to the local fast vars and perform a JUMP
        0. If it seems feasible, then bytecode ops will be
        injected. Otherwise, this method returns without modifying the
        code object.
        """

        # test for possible recursion optimization.

        # first check, make sure we've got a self-ref available. If we
        # don't then we cannot ensure it's actually a recursive call
        # at runtime, so a jump0 is unsafe.
        if self.self_ref is None:
            return

        # next, let's see if the parameters being passed line up with
        # the formals we're set up with

        parameters = gather_parameters(args, position)
        pos, kwds, vals, star, starstar = parameters

        if self.varargs or self.varkeywords or star or starstar:
            # no support for variadics, it's too tricky to inline. A
            # trampoline bounce isn't any slower than us calling to
            # other functions to figure out how to reform variadics
            # prior to a jump0, so just let the trampoline do its
            # bounce.
            return

        self_args = self.args

        # first, skim off the argument name bindings for the
        # positional arguments
        bindings = self_args[:len(pos)]

        # now we need to go through the keyword arguments in order and
        # record their binding
        for arg in kwds:
            if arg in bindings:
                # keyword parameter dups positional parameter name,
                # fall back on trampoline
                return
            elif arg not in self_args:
                # unknown keyword parameter, fall back on trampoline
                return
            else:
                bindings.append(arg)

        if len(self_args) != len(bindings):
            # if the bindings and argument count doesn't line up, then
            # again it's easier to just let the trampoline bounce and
            # have python figure out the args (and potentially raise a
            # TypeError). Default values take too much effort for now.
            return

        # if we made it this far, then we have a mapping of the
        # arguments we were given to the argument names, and thus can
        # evaluate in order and assign to the correct local variable,
        # then jump to 0, provided a quick runtime sanity check
        # passes, verifying that we are indeed calling the same
        # function that we're already executing.

        tclabel = self.gen_label()

        self.pseudop_dup()
        self.pseudop_get_var(self.self_ref)
        self.pseudop_compare_is()
        self.pseudop_pop_jump_if_false(tclabel)
        self.pseudop_pop()

        # evaluate all of the arguments in order
        for arg in pos:
            self.add_expression(arg, False)
        for arg in vals:
            self.add_expression(arg, False)

        # now bind them to the vars that we discovered
        for var in reversed(bindings):
            self.pseudop_set_var(var)

        self.pseudop_jump(0)
        self.pseudop_label(tclabel)


    def declare_tailcall(self):
        assert self.tco_enabled, "declare_tailcall without tco_enabled"
        assert not self.generator, "declare_tailcall with a generator"
        self.tailcalls += 1


    def _gensym_predicate(self, sym: symbol):
        # sym = str(sym)
        return (sym not in self.args and
                sym not in self.fast_vars and
                sym not in self.free_vars and
                sym not in self.cell_vars and
                sym not in self.global_vars)


    def gensym(self, name=None):
        """
        produces a deferral which will turn into a gensym call
        """

        return lazygensym(name, self._gensym_predicate)


    def pseudop_position_of(self, source_obj):
        """
        Inserts a position declaration based on the position data of the
        given source_obj (if possible)
        """

        position = get_position(source_obj, None)
        if position:
            assert (type(position) is tuple), "non-tuple position"
            self.pseudop_position(*position)


    def add_expression(self, expr, tc=False):
        """
        The short form for compiling an expression.
        """

        self.compile(expr, tc, None)


    def add_expression_with_return(self, expr):
        """
        Insert an expression, then an op to return its result from the
        current call
        """

        # a return is, by its very nature, always in the tailcall
        # position. Therefore we'll compile this as tailcall if we
        # have such enabled.
        self.compile(expr, True, None)

        # and insert the return op
        self.pseudop_return()


    def error(self, message, source):
        """
        Create a CompilerSyntaxError based on the message and source
        object. Will attempt to find position and line text based
        on the compiler's state.
        """

        text = None

        pos = None
        if is_pair(source) and source is not nil:
            pos = source.get_position()

        if pos and exists(self.filename):
            with open(self.filename, "rt") as fin:
                for text, _lineno in zip(fin, range(0, pos[0])):
                    # print(" ...", text)
                    pass

        if not text:
            text = str(source)
            if pos:
                pos = (pos[0], 0)

        return CompilerSyntaxError(message, pos, text=text,
                                   filename=self.filename)


    @contextmanager
    def tmp_compiled(self, tmp_env: Mapping):
        assert isinstance(tmp_env, Mapping)

        self.env_tmp_compiled.append(tmp_env)
        yield
        self.env_tmp_compiled.pop()


    def find_compiled(self, namesym: Symbol):
        """
        Search for and return a Compiled instance within the activated
        environment for this compiler. Returns None if nothing was
        found.
        """

        self.require_active()

        if is_lazygensym(namesym):
            # I might make this work some day, with macrolet, but for
            # now ... no.
            return None

        for tmp_env in reversed(self.env_tmp_compiled):
            if namesym in tmp_env:
                return tmp_env[namesym]
        else:
            return env_find_compiled(self.env, namesym)


def compile_expression(source_obj, env, filename="<anon>",
                       **codespace_args):

    """
    Compile and yield a Python code object representing the evaluation
    of the given source_obj expression, which should be the result of
    a a `read` call from a SourceStream, a valid symbol or cons pair,
    or a self-evaluating type.
    """

    factory = compiler_for_version()
    codespace = factory(filename=filename, **codespace_args)

    with codespace.active_context(env):
        codespace.add_expression_with_return(source_obj)
        code = codespace.complete()

    return code


def gather_formals(args, declared_at=None, filename=None):
    """
    parses formals pair args into five values:
    (positional, keywords, defaults, stararg, starstararg)

    - positional is a list of symbols defining positional arguments

    - defaults is a list of keywords and expr pairs defining keyword
      arguments and their default value expression

    - kwonly is a list of keywords and expr pairs which are
      keyword-only arguments and theid default value expression

    - stararg is a symbol for variadic positional arguments

    - starstararg is a symbol for variadic keyword arguments
    """

    undefined = object()

    err = partial(SibilantSyntaxError,
                  location=declared_at,
                  filename=filename)

    if is_symbol(args):
        return ((), (), (), args, None)

    elif isinstance(args, (list, tuple)):
        improper = False
        args = cons(*args, nil)

    elif is_proper(args):
        improper = False

    elif is_pair(args):
        improper = True

    else:
        raise err("formals must be symbol or pair, not %r" % args)

    positional = []

    iargs = iter(args.unpack())
    for arg in iargs:
        if is_keyword(arg):
            if improper:
                raise err("cannot mix improper formal with keywords")
            else:
                break
        elif is_symbol(arg) or is_lazygensym(arg):
            positional.append(arg)
        else:
            raise err("positional formals must be symbols, nor %r" % arg)
    else:
        # handled all of args, done deal.
        if improper:
            return (positional[:-1], (), (), positional[-1], None)
        else:
            return (positional, (), (), None, None)

    defaults = []
    kwonly = []

    while arg not in (_keyword_star, _keyword_starstar):
        value = next(iargs, undefined)
        if value is undefined:
            raise err("missing value for keyword formal %s" % args)
        else:
            defaults.append((arg, value))

        arg = next(iargs, undefined)

        if arg is undefined:
            break
        elif is_keyword(arg):
            continue
        else:
            raise err("keyword formals must be alternating keywords and"
                      " values, not %r" % arg)

    star = None
    starstar = None

    if arg is undefined:
        return (positional, defaults, kwonly, None, None)

    if arg is _keyword_star:
        star = next(iargs, undefined)
        if star is undefined:
            raise err("* keyword requires symbol binding")
        elif star is nil:
            # nil means an ignored star arg, this is allowed.
            pass
        elif not (is_symbol(star) or is_lazygensym(star)):
            raise err("* keyword requires symbol binding, not %r" % star)
        arg = next(iargs, undefined)

    if arg is undefined:
        return (positional, defaults, kwonly, star, starstar)

    # while is_symbol(arg):
    #     kwonly.append(arg)
    #     arg = next(iargs, undefined)
    #
    # if arg is undefined:
    #     return (positional, defaults, kwonly, star, starstar)

    if not is_keyword(arg):
        raise err("expected keyword in formals, got %r" % arg)

    # keyword formals after *: are considered keyword-only
    while arg not in (_keyword_star, _keyword_starstar):
        value = next(iargs, undefined)
        if value is undefined:
            raise err("missing value for keyword-only formal %s" % arg)
        else:
            kwonly.append((arg, value))

        arg = next(iargs, undefined)
        if arg is undefined:
            break
        elif is_keyword(arg):
            continue
        else:
            raise err("keyword-only formals must be alternating keywords"
                      " and values, not %r" % arg)

    if arg is _keyword_starstar:
        starstar = next(iargs, undefined)
        if starstar is undefined:
            raise err("** keyword requires symbol binding")
        elif not (is_symbol(starstar) or is_lazygensym(starstar)):
            raise err("** keyword requires symbol binding, not %r" % star)
        arg = next(iargs, undefined)

    if arg is not undefined:
        raise err("leftover formals %r" % arg)

    return (positional, defaults, kwonly, star, starstar)


def simple_parameters(source_args, declared_at=None):
    parameters = gather_parameters(source_args, declared_at)
    pos, kwds, vals, star, starstar = parameters

    args = list(pos)
    if star:
        args.extend(star)

    kwargs = dict(zip(map(str, kwds), vals))
    if starstar:
        kwargs.update(starstar)

    return args, kwargs


def gather_parameters(args, declared_at=None, filename=None):
    """
    parses parameter args into five values:
    (positional, keywords, values, stararg, starstararg)

    - positional is a list of expressions for positional arguments
    - keywords is a list of keywords defining keyword arguments
    - values is a list of expressions defining values for keywords
    - stararg is a symbol for variadic positional expression
    - starstararg is a symbol for variadic keyword expression
    """

    undefined = object()

    def err(msg):
        return SibilantSyntaxError(msg, location=declared_at,
                                   filename=filename)

    if is_symbol(args) or is_lazygensym(args):
        return ((), (), (), args, None)

    elif isinstance(args, (list, tuple)):
        improper = False
        args = cons(*args, nil) if args else nil

    elif is_proper(args):
        improper = False

    elif is_pair(args):
        improper = True

    else:
        raise err("parameters must be symbol or pair, not %r" % args)

    positional = []

    iargs = iter(args.unpack())
    for arg in iargs:
        if is_keyword(arg):
            break
        else:
            positional.append(arg)
    else:
        # handled all of args, done deal.
        if improper:
            return (positional[:-1], (), (), positional[-1], None)
        else:
            return (positional, (), (), None, None)

    keywords = []
    defaults = []

    while arg not in (_keyword_star, _keyword_starstar):
        keywords.append(arg)

        value = next(iargs, undefined)
        if value is undefined:
            raise err("missing value for keyword parameter %s" % arg)
        else:
            defaults.append(value)

        arg = next(iargs, undefined)
        if arg is undefined:
            break
        elif is_keyword(arg):
            continue
        else:
            raise err("keyword parameters must be alternating keywords and"
                      " values, not %r" % arg)

    star = None
    starstar = None

    if arg is undefined:
        return (positional, keywords, defaults, None, None)

    if arg is _keyword_star:
        star = next(iargs, undefined)
        if star is undefined:
            raise err("* keyword parameter needs value")
        arg = next(iargs, undefined)

    if arg is _keyword_starstar:
        starstar = next(iargs, undefined)
        if starstar is undefined:
            raise err("** keyword parameter needs value")
        arg = next(iargs, undefined)

    if arg is not undefined:
        raise err("leftover parameters %r" % arg)

    return (positional, keywords, defaults, star, starstar)


def unpack_formals(args, kwds,
                   positionals, variadic, kwonly,
                   keywords, defaults, kwvariadic):

    lpos = len(positionals)
    largs = len(args)
    kwvar = dict(kwds)

    if lpos == largs:
        args = list(args)
        var = () if variadic else None
    elif lpos < largs:
        if variadic:
            args = list(args[:lpos])
            var = args[lpos:]
        else:
            raise TypeError("too many positional arguments")
    else:
        args = list(args)
        var = () if variadic else None
        try:
            for a in positionals[largs:]:
                args.append(kwvar.pop(a))
        except KeyError:
            raise TypeError("missing required argument %s" % a)

    kwds = {}
    try:
        for a in kwonly:
            kwds[a] = kwvar.pop(a)
    except KeyError:
        raise TypeError("missing required keyword-only argument %s" % a)

    for a in keywords:
        kwds[a] = kwvar.pop(a, defaults[a])

    if kwvar and not kwvariadic:
        raise TypeError("unexpected arguments, %r" % list(kwvar.keys()))

    return args, var, kwds, (kwvar if kwvariadic else None)


def env_find_compiled(env, namesym):
    """
    Search for a Compiled instance in the given environment and its
    __builtins__ if any exist. Returns None if nothing was found.
    """

    # assert is_symbol(namesym)

    if not isinstance(env, Mapping):
        env = vars(env)

    # okay, let's look through the environment by name
    name = str(namesym)

    # is it in globals?
    if name in env:
        found = env[name]
        return found if is_compiled(found) else None

    # maybe in builtins?
    if "__builtins__" in env:
        env = env["__builtins__"].__dict__
        if name in env:
            found = env[name]
            return found if is_compiled(found) else None

    # nope
    return None


def env_get_expander(env, source_obj):
    """
    Find an expander function for the given source obj in the
    specified environment.
    """

    if not isinstance(env, Mapping):
        env = vars(env)

    expander = None

    if source_obj is nil:
        return None

    elif is_symbol(source_obj):
        namesym = source_obj
        found = env_find_compiled(env, namesym)
        if is_alias(found):
            expander = found.expand

    elif is_proper(source_obj):
        namesym, params = source_obj

        if is_symbol(namesym):
            found = env_find_compiled(env, namesym)
            if is_alias(found):
                def expander():
                    expanded = cons(found.expand(), params)
                    expanded.set_position(source_obj.get_position())
                    return expanded

            elif is_macro(found):
                if found._proper:
                    # FIXME: this is some garbage right here. we need
                    # to make sure macros aren't being invoked this
                    # way with a variadic.
                    position = params.get_position()
                    args, kwargs = simple_parameters(params, position)
                    expander = partial(found.expand, *args, **kwargs)
                else:
                    expander = partial(found.expand, *params.unpack())

    return expander


def iter_macroexpand(env, source_obj, position=None):
    if env is None:
        compiler = current()
        env = None if compiler is None else compiler.env

    if env is None:
        raise CompilerException("macroexpand requires non-None env when"
                                " no compiler is active")

    if position is None and is_pair(source_obj):
        position = source_obj.get_position()

    expander = env_get_expander(env, source_obj)
    while expander:
        expanded = expander()
        yield expanded

        expander = env_get_expander(env, expanded)
        if is_pair(expanded):
            fill_position(expanded, position)


def macroexpand(env, source_obj, position=None):
    """

    """

    for source_obj in iter_macroexpand(env, source_obj, position):
        pass
    return source_obj


def macroexpand_1(env, source_obj, position=None):
    return next(iter_macroexpand(env, source_obj, position), source_obj)


#
# The end.
