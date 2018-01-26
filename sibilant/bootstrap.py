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
Pythonic builtin definitions for sibilant.

These are used to bootstrap an importer that can load the
_builtins.lspy module

This module and _builtins are then merged together to create the final
builtins module, which is made available to any module being loaded
via sibilant.

This contains re-bindings of common existing Python functions,
sometimes just under a slightly more lisp-ish name, but in some cases
also altered to handle the pair type in a sensible way.

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


def __setup__(glbls):

    # since this is the basis for the builtins module (ie. the default
    # available namespace for everything sibilant executes), we need
    # to be fastidious about what ends up in it. We need to avoid
    # leaking temporary variable, or imports, etc. This setup function
    # will bind various objects to the passed glbls dictionary, and
    # return an __all__ tuple.

    import sys

    from copy import copy, deepcopy
    from fractions import Fraction as fraction
    from functools import partial, reduce, wraps

    import sibilant
    import sibilant.compiler as compiler
    import sibilant.tco as tco
    import sibilant.specials as specials
    import sibilant.operators as operators

    from sibilant import is_pair, TypePredicate


    _all_ = []


    def _op(opf, name=None, rename=False):
        name = name if name else opf.__name__

        if rename:
            opf.__name__ = name
            opf.__qualname__ = "sibilant.builtins." + name

        glbls[name] = opf
        _all_.append(name)


    def _val(value, name):
        glbls[name] = value
        _all_.append(name)


    def _ty(type_, name):
        _val(type_, name)
        _op(TypePredicate(name + "?", type_), None, True)


    # === mass re-export from other modules ==

    sd = specials.__dict__
    for name in specials.__all__:
        glbls[name] = sd[name]
        _all_.append(name)

    sd = operators.__dict__
    for name in operators.__all__:
        glbls[name] = sd[name]
        _all_.append(name)

    # all the exceptions from builtins. We'll pick other values more
    # selectively later
    for name, value in __builtins__.items():
        if isinstance(value, type) and issubclass(value, BaseException):
            glbls[name] = value
            _all_.append(name)


    # == sibilant data types ===

    _op(sibilant.cons, "cons")
    _op(sibilant.car, "car")
    _op(sibilant.setcar, "set-car")
    _op(sibilant.cdr, "cdr")
    _op(sibilant.setcdr, "set-cdr")
    _op(sibilant.is_pair, "pair?")
    _op(sibilant.is_proper, "proper?")
    _op(sibilant.build_proper, "build-proper")
    _val(sibilant.nil, "nil")
    _op(sibilant.is_nil, "nil?")
    _val(sibilant.symbol, "symbol")
    _op(sibilant.is_symbol, "symbol?")
    _val(sibilant.keyword, "keyword")
    _op(sibilant.is_keyword, "keyword?")

    _op(sibilant.merge_pairs, "merge_pairs")
    _op(sibilant.build_unpack_pair, "build-unpack-pair")

    _op(sibilant.reapply, "reapply")

    _op(sibilant.first, "first")
    _op(sibilant.second, "second", rename=True)
    _op(sibilant.third, "third", rename=True)
    _op(sibilant.fourth, "fourth", rename=True)
    _op(sibilant.fifth, "fifth", rename=True)
    _op(sibilant.sixth, "sixth", rename=True)
    _op(sibilant.seventh, "seventh", rename=True)
    _op(sibilant.eighth, "eighth", rename=True)
    _op(sibilant.ninth, "ninth", rename=True)
    _op(sibilant.tenth, "tenth", rename=True)

    _op(sibilant.last, "last")


    # === sibilant compiler builtins ===

    _val(compiler.Special, "special")
    _op(compiler.is_special, "special?")

    _val(compiler.Macro, "macro")
    _op(compiler.is_macro, "macro?")

    _val(compiler.Alias, "alias")
    _op(compiler.is_alias, "alias?")

    _val(compiler.Operator, "operator")
    _op(compiler.is_operator, "operator?")

    _op(tco.trampoline, "trampoline")
    _op(tco.tailcall, "tailcall")
    _op(tco.tco_disable, "tco-disable")

    _op(compiler.current, "active-compiler")


    # === some python builtin types ===

    _ty(partial, "partial")
    _op(wraps, "wraps")
    _op(copy, "copy")
    _op(deepcopy, "deep-copy")


    def _as_tuple(value):
        """
        (to-tuple VALUE)

        Converts VALUE to a tuple. If VALUE is a cons pair, will
        unpack it. Otherwise, VALUE will be iterated over and its
        contents collected.
        """

        if is_pair(value):
            return tuple(value.unpack())
        else:
            return tuple(value)

    _op(_as_tuple, "to-tuple", rename=True)


    def _as_list(value):
        """
        (to-list VALUE)

        Converts VALUE to a list. If VALUE is a cons pair, will
        unpack it. Otherwise, VALUE will be iterated over and its
        contents collected.
        """

        if is_pair(value):
            return list(value.unpack())
        else:
            return list(value)

    _op(_as_list, "to-list", rename=True)


    def _as_set(value):
        """
        (to-set VALUE)

        Converts VALUE to a set. If VALUE is a cons pair, will
        unpack it. Otherwise, VALUE will be iterated over and its
        contents collected.
        """

        if is_pair(value):
            return set(value.unpack())
        else:
            return set(value)

    _op(_as_set, "to-set", rename=True)


    def _count(value):
        """
        (count VALUE)

        The number of items in VALUE. If VALUE is a cons pair, will
        unpack it. Otherwise, identical to (len VALUE)
        """

        return value.count() if is_pair(value) else len(value)

    _op(_count, "count", rename=True)


    def apply(fun, arglist=(), kwargs={}):
        """
        (apply FUN)
        (apply FUN arglist: POSITIONALS)
        (apply FUN kwargs: KEYWORDS)
        (apply FUN arglist: POSITIONALS kwargs: KEYWORDS)

        Invokes FUN as a function, with optional iterable
        POSITIONALS as positional arguments, and optional mapping
        KEYWORDS as keyword arguments.

        If POSITIONALS is a a cons pair, it will be unpacked instead of
        iterated
        """

        # this doesn't need to be an operator or a special, because in
        # instances where this could be compiled inline, someone could
        # just as easily write (FUN *: POSITIONALS **: KEYWORDS) and
        # have it be inlined. The only real use of apply is as a
        # runtime function, passed along to call its arguments.

        if is_pair(arglist):
            arglist = arglist.unpack()
        return fun(*arglist, **kwargs)

    _op(apply, "apply", rename=True)


    # _op(sibilant.build_tuple, "values")
    # _op(sibilant.build_tuple, "build-tuple")
    _ty(tuple, "tuple")

    # _op(sibilant.build_list, "build-list")
    _ty(list, "list")

    # _op(sibilant.build_dict, "build-dict")
    _ty(dict, "dict")

    # _op(sibilant.build_set, "build-set")
    _ty(set, "set")

    _op(lambda value: hasattr(value, "__iter__"),
        "iterable?", rename=True)


    # === some python builtin functions ===

    _ty(map, "map")
    _ty(zip, "zip")
    _ty(filter, "filter")
    _op(reduce, "reduce")
    _ty(enumerate, "enumerate")

    _op(callable, "callable?")
    _op(next, "next")
    _op(len, "len")
    _op(format, "format")
    _op(getattr, "getattr")
    _op(setattr, "setattr")
    _op(isinstance, "isinstance")
    _op(open, "open")
    _op(print, "print")

    _ty(object, "object")
    _ty(str, "bytes")
    _ty(str, "str")
    _ty(type, "type")
    _ty(bool, "bool")
    _ty(int, "int")
    _ty(float, "float")
    _ty(complex, "complex")
    _ty(fraction, "fraction")
    _ty(range, "range")
    _ty(slice, "slice")
    _op(chr, "chr")
    _op(ord, "ord")
    _op(min, "min")
    _op(max, "max")

    _op(repr, "repr")
    _op(help, "help")
    _op(dir, "dir")

    _op(__import__, "import")
    _op(globals, "globals")
    _op(locals, "locals")
    _op(compile, "py-compile")
    _op(eval, "py-eval")

    _op(sys.exit, "exit")


    # done with setup
    return tuple(_all_)


# --- and finally, clean up ---

__all__ = __setup__(globals())
del __setup__


#
# The end.
