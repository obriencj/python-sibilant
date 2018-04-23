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
    from decimal import Decimal as decimal
    from functools import partial, reduce, wraps

    import sibilant.lib as lib
    import sibilant.compiler as compiler
    import sibilant.tco as tco
    import sibilant.specials as specials
    import sibilant.operators as operators

    from sibilant.parse import Atom as atom


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
        _op(lib.TypePredicate(name + "?", type_), None, True)


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

    _ty(lib.pair, "pair")
    _op(lib.cons, "cons")
    _op(lib.car, "car")
    _op(lib.setcar, "set-car")
    _op(lib.cdr, "cdr")
    _op(lib.setcdr, "set-cdr")
    _op(lib.is_proper, "proper?")
    _op(lib.is_recursive, "recursive?")
    _op(lib.build_proper, "build-proper")
    _op(lib.unpack, "unpack")
    _val(lib.nil, "nil")
    _op(lib.is_nil, "nil?")

    _ty(lib.symbol, "symbol")
    _ty(lib.keyword, "keyword")

    _op(lib.build_unpack_pair, "build-unpack-pair")

    _op(lib.reapply, "reapply")

    _ty(lib.values, "values")

    _op(lib.repeatedly, "repeatedly")

    _op(lib.first, "first")
    _op(lib.second, "second", rename=True)
    _op(lib.third, "third", rename=True)
    _op(lib.fourth, "fourth", rename=True)
    _op(lib.fifth, "fifth", rename=True)
    _op(lib.sixth, "sixth", rename=True)
    _op(lib.seventh, "seventh", rename=True)
    _op(lib.eighth, "eighth", rename=True)
    _op(lib.ninth, "ninth", rename=True)
    _op(lib.tenth, "tenth", rename=True)

    _op(lib.last, "last")


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

    _ty(atom, "atom")


    # === some python builtin types ===

    _ty(partial, "partial")
    _op(wraps, "wraps")
    _op(copy, "copy")
    _op(deepcopy, "deep-copy")

    _ty(tuple, "tuple")
    _ty(list, "list")
    _ty(dict, "dict")
    _ty(set, "set")
    _ty(frozenset, "frozenset")

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
    _op(input, "input")
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
    _ty(decimal, "decimal")
    _ty(range, "range")
    _ty(memoryview, "memoryview")
    _ty(slice, "slice")

    _op(sorted, "sorted")
    _ty(reversed, "reversed")

    _op(chr, "chr")
    _op(ord, "ord")
    _op(min, "min")
    _op(max, "max")
    _op(abs, "abs")
    _op(oct, "oct")
    _op(hex, "hex")
    _op(all, "all")
    _op(any, "any")

    _op(hash, "hash")
    _op(super, "super")

    _op(repr, "repr")
    _op(help, "help")
    _op(dir, "dir")
    _op(vars, "vars")

    _op(__import__, "py-import")
    _op(__import__, "__import__")
    _op(globals, "globals")
    _op(locals, "locals")
    _op(compile, "py-compile")
    _op(eval, "py-eval")

    _op(sys.exit, "exit")

    _val(sys, "sys")
    # _val(__debug__, "__debug__")

    # done with setup
    # return tuple(_all_)
    return None


# --- and finally, clean up ---


__setup__(globals())

try:
    del __setup__     # noqa
    del __file__      # noqa
    del __builtins__  # noqa
except NameError:
    pass


#
# The end.
