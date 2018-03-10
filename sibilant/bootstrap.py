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
sibilant.bootstrap

Pythonic builtin definitions for sibilant.

These are used to bootstrap an importer that can load the
basics.lspy module

bootstrap and basica are then merged together to create the final
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
        _op(sibilant.TypePredicate(name + "?", type_), None, True)


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

    _ty(sibilant.pair, "pair")
    _op(sibilant.cons, "cons")
    _op(sibilant.car, "car")
    _op(sibilant.setcar, "set-car")
    _op(sibilant.cdr, "cdr")
    _op(sibilant.setcdr, "set-cdr")
    _op(sibilant.is_proper, "proper?")
    _op(sibilant.build_proper, "build-proper")
    _val(sibilant.nil, "nil")
    _op(sibilant.is_nil, "nil?")

    _ty(sibilant.symbol, "symbol")
    _ty(sibilant.keyword, "keyword")

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

    _ty(compiler.Special, "special")
    _ty(compiler.Macro, "macro")
    _ty(compiler.Alias, "alias")
    _ty(compiler.Operator, "operator")

    _op(tco.trampoline, "trampoline")
    _op(tco.tailcall, "tailcall")
    _op(tco.tailcall_disable, "tailcall-disable")
    _op(tco.tailcall_enable, "tailcall-enable")

    _op(compiler.current, "active-compiler")


    # === some python builtin types ===

    _ty(partial, "partial")
    _op(wraps, "wraps")
    _op(copy, "copy")
    _op(deepcopy, "deep-copy")

    _ty(tuple, "tuple")
    _ty(list, "list")
    _ty(dict, "dict")
    _ty(set, "set")

    _op(lambda value: hasattr(value, "__iter__"),
        "iterable?", rename=True)


    # === some python builtin functions ===

    _ty(map, "map")
    _ty(zip, "zip")
    _ty(filter, "filter")
    _ty(enumerate, "enumerate")

    _op(reduce, "reduce")

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
