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

from abc import abstractmethod
from contextlib import contextmanager
from functools import partial
from os.path import exists
from platform import python_implementation
from sys import version_info
from types import CodeType
from typing import Union

from ..lib import (
    SibilantException, SibilantSyntaxError,
    symbol, is_symbol,
    lazygensym, is_lazygensym,
    keyword, is_keyword,
    cons, nil, is_pair, is_proper,
    get_position, fill_position,
)

from ..pseudops import CodeFlag, CodeSpace, Mode, Opcode, Pseudop, Block


__all__ = (
    "UnsupportedVersion",
    "CompilerSyntaxError",
    "CodeSpace", "CodeFlag", "Block", "Mode", "Opcode", "Pseudop",
    "SibilantCompiler",
    "code_space_for_version",
    "Compiled", "is_compiled",
    "Special", "is_special",
    "Macro", "is_macro",
    "Alias", "is_alias",
    "Operator", "is_operator",
    "gather_formals", "gather_parameters",
)


Symbol = Union[lazygensym, symbol]


_keyword_star = keyword("*")
_keyword_starstar = keyword("**")


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


class Compiled():
    __slots__ = ("__name__", )
    __objname__ = "sibilant compiled"


    def __init__(self, name):
        self.__name__ = name


    def __call__(self, *args, **kwds):
        msg = "Attempt to call %r as a runtime function" % self
        raise TypeError(msg)


    def __repr__(self):
        return "<%s %r>" % (self.__objname__, self.__name__)


    def compile(self, compiler, source_obj):
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
            "__compile__": staticmethod(compilefn),
        }
        cls = type(nom, (cls, ), mbs)
        return object.__new__(cls)


    @trampoline
    def compile(self, source, tc=False):
        return tailcall(self.__compile__)(source, tc)


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
    def compile(self, compiler, source_obj, tc=False):
        called_by, source = source_obj

        if self._proper:
            position = source_obj.get_position()
            args, kwargs = simple_parameters(source, position)
            expr = self.expand(*args, **kwargs)

        else:
            expr = self.expand(*source.unpack())

        fill_position(expr, source_obj.get_position())
        return tailcall(compiler.compile)(expr, tc)


def is_macro(obj):
    return isinstance(obj, Macro)


class Alias(Macro):
    __objname__ = "alias"


    def compile(self, compiler, source_obj, tc=False):
        expanded = self.expand()
        expanded = _symbol_None if expanded is None else expanded

        if is_pair(source_obj):
            called_by, source = source_obj
            res = cons(expanded, source)
            fill_position(res, source_obj.get_position())
            expanded = res

        return tailcall(compiler.compile)(expanded, tc)


def is_alias(obj):
    return isinstance(obj, Alias)


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
            "__compile__": staticmethod(compilefn),
        }
        cls = type(nom, (cls, ), mbs)
        return object.__new__(cls)


    @trampoline
    def compile(self, source, tc=False):
        return tailcall(self.__compile__)(source, tc)


def is_operator(obj):
    return isinstance(obj, Operator)


# these types alone are valid constant value types when marshalling a
# code object. Outside of marshalling, Python doesn't seem to care
# what you put in the consts tuple of a code object.
_CONST_TYPES = (
    CodeType,
    str, bytes,
    tuple, list, dict, set,
    bool, int, float, complex,
    type(None), type(...),
)


# a bunch of commonly used symbols, so we don't have to try and
# recreate over and over.

_symbol_nil = symbol("nil")
_symbol_None = symbol("None")
_symbol_True = symbol("True")
_symbol_False = symbol("False")
_symbol_ellipsis = symbol("...")

_symbol_attr = symbol("attr")


def _label_generator(formatstr="label_%04x"):
    counter = 0

    def gen_label():
        nonlocal counter
        counter += 1
        return formatstr % counter

    return gen_label


def compiler_version(ver=version_info,
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
            from .cpython37 import SibilantCPython37
            return SibilantCPython37

        elif (3, 6) <= ver <= (3, 7):
            from .cpython36 import SibilantCPython36
            return SibilantCPython36

        elif (3, 5) <= ver <= (3, 6):
            from .cpython35 import SibilantCPython35
            return SibilantCPython35

    raise UnsupportedVersion(ver, impl)


class SibilantCompiler(PseudopCompiler, metaclass=ABCMeta):


    def __init__(self, options):
        pass


    @abstractmethod
    def activate(self, environment):
        pass


    def complete(self):
        pass


    @trampoline
    def compile(self, source_obj, tc=False):
        if is_pair(source_obj):
            dispatch = self.compile_pair
        elif is_atom(source_obj):
            dispatch = self.compile_atom
        elif is_symbol(source_obj):
            dispatch = self.compile_symbol
        elif is_keyword(source_obj):
            dispatch = self.compile_keyword
        elif isinstance(source_obj, _CONST_TYPES):
            dispatch = self.compile_constant
        else:
            msg = "Unsupported source object {:r}".format(source_obj)
            raise self._err(msg, source_obj)

        return tailcall(dispatch)(source_obj, tc)


    @trampoline
    def compile_pair(self, source_obj: pair, tc=False):
        if not is_proper(source_obj):
            msg = "cannot evaluate improper lists as expressions"
            raise self._err(msg, source_obj)

        self.psedop_position_of(source_obj)
        position = source_obj.get_position()
        head, tail = source_obj

        if is_symbol(head) or is_lazygensym(head):
            comp = self.find_compiled(head)
            if comp:
                return tailcall(comp.compile)(self, expr, tc)

        return tailcall(self.compile_apply)(self, expr, tc)


    @trampiline
    def compile_apply(self, source_obj: pair, tc=False):
        pass


    @trampoline
    def compile_atom(self, source_obj, tc=False):
        pattern = self.find_atom_pattern(source_obj)
        if pattern is None:
            pattern = symbol

        self.pseudop_position_of(source_obj)
        source = pattern(source_obj)

        return tailcall(self.compile)(source, tc)


    @trampoline
    def compile_symbol(self, sym: Symbol, tc=False):
        comp = self.find_compiled(head)
        if comp:
            return tailcall(comp.compile)(self, sym, tc)

        elif is_lazygensym(sym):
            return self.pseudop_get_var(sym)

        elif sym is _symbol_None:
            return self.pseudop_const(None)

        elif sym is _symbol_True:
            return self.pseudop_const(True)

        elif sym is _symbol_False:
            return self.pseudop_const(False)

        elif sym is _symbol_ellipsis:
            return self.pseudop_const(...)

        else:
            ex = sym.rsplit(".", 1)
            if len(ex) == 1:
                return self.pseudop_get_var(sym)
            else:
                source = cons(_symbol_attr, *ex, nil)
                return tailcall(self.compile)(source, tc)


    @trampoline
    def compile_keyword(self, kwd: keyword, tc=False):
        source = cons(_symbol_keyword, str(kwd), nil)
        return tailcall(self.compile)(source, False)


    @trampoline
    def compile_constant(self, value: Constant, tc=False):
        return self.pseudop_const(value)


class ExpressionCodeSpace(CodeSpace):
    """
    Adds support for expressions, operators, macros, and special forms
    to the basic functionality of CodeSpace
    """


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


    def child_context(self, **kwargs):
        """
        Returns an active context for a child codespace
        """

        self.require_active()
        cs = self.child(**kwargs)
        return cs.active_context(self.env)


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


    def pseudop_position_of(self, cl):
        position = get_position(cl, None)
        if position:
            assert (type(position) is tuple), "non-tuple position"
            self.pseudop_position(*position)


    def add_expression(self, expr, tc=False):
        """
        Insert an expression into the code space. expr should be a cons
        cell representing the expression. If the expression appears to
        be a special form (either a macro defined in the CodeSpace's
        env, or a pre-defined built-in special), it will be expanded
        and compiled to pseudo ops.
        """

        tc = tc and self.tco_enabled

        self.require_active()

        if expr is None:
            self.pseudop_const(None)
            return

        self.pseudop_position_of(expr)

        while expr is not None:
            if expr is nil:
                # convert nil expressions to a literal nil
                self.pseudop_get_var(symbol("nil"))
                expr = None

            elif is_pair(expr):
                try:
                    expr = self.compile_pair(expr, tc)
                except TypeError as te:
                    msg = "while compiling pair %r" % expr
                    raise self.error(msg, expr) from te

            elif is_symbol(expr) or is_lazygensym(expr):
                try:
                    expr = self.compile_symbol(expr, tc)
                except TypeError as te:
                    msg = "while compiling symbol %r" % expr
                    raise self.error(msg, expr) from te

            elif is_keyword(expr):
                expr = self.compile_keyword(expr)

            else:
                # TODO there are some literal types that can't be used
                # as constants, will need to fill those in here. For
                # now we're just presuming it's going to be a
                # constant, the end.
                expr = self.pseudop_const(expr)

            if is_compiled(expr):
                msg = "leftover higher-order macro %r" % expr
                raise CompilerException(msg)

        return None


    def add_expression_with_return(self, expr):
        """
        Insert an expression, then an op to return its result from the
        current call
        """
        self.add_expression(expr)
        self.pseudop_return()


    def compile_pair(self, expr, tc=False):
        self.require_active()

        if not is_proper(expr):
            print("compile_pair improper:", str(expr))
            raise self.error("cannot evaluate improper lists as expressions",
                             expr)

        self.pseudop_position_of(expr)
        position = expr.get_position()
        head, tail = expr

        if is_symbol(head):
            # see if this is a a compiled call
            comp = self.find_compiled(head)
            if comp:
                # yup. We'll just report that we've expanded to that
                return comp.compile(self, expr, tc)

            head = self.compile_symbol(head)
            if head is None:
                # fall out of this nonsense
                pass
            elif is_compiled(head):
                # head evaluated at compile-time to a higher-order macro
                namesym = symbol(head.__name__)
                return head.compile(self, cons(namesym, tail), tc)
            else:
                return cons(head, tail)

        elif is_proper(head):
            head = self.compile_pair(head)
            if head is None:
                # fall out of this nonsense
                pass
            elif is_compiled(head):
                # head evaluated at compile-time to a higher-order macro
                namesym = symbol(head.__name__)
                return head.compile(self, cons(namesym, tail), tc)
            else:
                return cons(head, tail)

        else:
            # head was neither a proper nor a symbol... wtf was it?
            # probably an error, so let's try and actually add it as
            # an expression and let it blow up.
            self.add_expression(head, tc)

        # if we made it this far, head has already been compiled and
        # returned None (meaning it was just a plain-ol expression),
        # so we can proceed with normal apply semantics
        self.compile_call_tos(tail, position, tc)

        return None


    def compile_call_tos(self, args, position=None, tc=False):
        if tc:
            self.helper_tailcall_tos(args, position)
        self.helper_compile_call(args, position)


    def helper_tailcall_tos(self, args, position):
        """
        Should be invoked upon TOS functions objects that could
        potentially recur. tc indicates whether the TOS is in a valid
        tailcall position.
        """

        if not self.tco_enabled or \
           self.generator or \
           self.mode is Mode.MODULE:
            return False

        self.helper_tailrecur_tos(args, position)

        # either this isn't a self-referential function, or it is and
        # the tailcall isn't recursive, so we'll use the trampoline
        # instead.
        self.pseudop_get_var(symbol("tailcall"))
        self.pseudop_rot_two()
        self.pseudop_call(1)

        self.declare_tailcall()


    def helper_tailrecur_tos(self, args, position):
        # test for possible recursion optimization.

        # first check, make sure we've got a self-ref available. If we
        # don't then we cannot ensure it's actually a recursive call
        # at runtime, so a jump0 is unsafe.
        if not (self.free_vars and self.free_vars[0] is symbol("")):
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
        for arg in map(str, kwds):
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
        self.pseudop_get_var(symbol(""))
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


    @abstractmethod
    def helper_compile_call(self, tail, position):
        """
        The function to be called is presumed to already be on the stack
        before this is helper is invoked. The helper should assemble the
        arguments as necessary on the stack, and then push the pseudops
        to evaluate them and finally to call the function.
        """

        pass


    def compile_symbol(self, sym: Symbol, tc=False):
        """
        The various ways that a symbol on its own can evaluate.
        """

        # assert (is_symbol(sym))

        self.require_active()

        if is_lazygensym(sym):
            return self.pseudop_get_var(sym)

        comp = self.find_compiled(sym)
        if comp and is_alias(comp):
            return comp.compile(self, sym, tc)

        elif sym is _symbol_None:
            return self.pseudop_const(None)

        elif sym is _symbol_True:
            return self.pseudop_const(True)

        elif sym is _symbol_False:
            return self.pseudop_const(False)

        elif sym is _symbol_ellipsis:
            return self.pseudop_const(...)

        else:
            ex = sym.rsplit(".", 1)
            if len(ex) == 1:
                return self.pseudop_get_var(sym)
            else:
                return cons(_symbol_attr, *ex, nil)


    def compile_keyword(self, kwd: keyword):
        # it should be fairly rare that a keyword is actually
        # passed anywhere at runtime -- it's mostly meant for use
        # as a marker in source expressions for specials.

        # assert is_keyword(kwd)

        self.require_active()

        self.pseudop_get_var(symbol("keyword"))
        self.pseudop_const(str(kwd))
        self.pseudop_call(1)
        return None


    def error(self, message, source):

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
            pos = (pos[0], 0)

        return CompilerSyntaxError(message, pos, text=text,
                                   filename=self.filename)


    def find_compiled(self, namesym: Symbol):
        if is_lazygensym(namesym):
            return None
        else:
            return _find_compiled(self.env, namesym)


def compile_expression(source_obj, env, filename="<anon>",
                       **codespace_args):

    """
    Compile and yield a Python code object representing the evaluation
    of the given source_obj expression, which should be the result of
    a a `read` call from a SourceStream, a valid symbol or cons pair,
    or a self-evaluating type.
    """

    factory = code_space_for_version()
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

    def err(msg):
        return SibilantSyntaxError(msg, location=declared_at,
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


def _find_compiled(env, namesym: symbol):
    assert is_symbol(namesym)

    # okay, let's look through the environment by name
    name = str(namesym)

    try:
        # is it in globals?
        found = env[name]
    except KeyError:
        try:
            # nope, how about in builtins?
            env = env["__builtins__"].__dict__
            found = env[name]

        except KeyError:
            # nope
            found = None

    if found and is_compiled(found):
        # we found a Macro instance, return the relevant
        # method
        return found

    else:
        # we either found nothing, or what we found doesn't
        # qualify
        return None


def _get_expander(env, source_obj):
    expander = None

    if source_obj is nil:
        return None

    elif is_symbol(source_obj):
        namesym = source_obj
        found = _find_compiled(env, namesym)
        if is_alias(found):
            expander = found.expand

    elif is_proper(source_obj):
        namesym, params = source_obj

        if is_symbol(namesym):
            found = _find_compiled(env, namesym)
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

    expander = _get_expander(env, source_obj)
    while expander:
        expanded = expander()
        yield expanded

        expander = _get_expander(env, expanded)
        if is_pair(expanded):
            fill_position(expanded, position)


def macroexpand(env, source_obj, position=None):
    for source_obj in iter_macroexpand(env, source_obj, position):
        pass
    return source_obj


def macroexpand_1(env, source_obj, position=None):
    return next(iter_macroexpand(env, source_obj, position), source_obj)


#
# The end.
