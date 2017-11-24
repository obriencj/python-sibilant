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
sibilant.compiler.tco

Ultra Simple Tail-Calls via Trampoline

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


__all__ = (
    "trampoline", "tailcall", "tco_disable",
)


def setup():
    # because this is going to be invoked so frequently, it becomes
    # important that the individual trampoline and tailcall functions
    # are as fast as I can make them in Python. We also want to hide
    # the TailCall class so that it remains a good sentinel value. So
    # we'll create that class and the big three functions within this
    # setup function.


    from functools import partial, partialmethod
    _getattr = getattr
    _type = type


    class TailCall(partial):
        # invocation of partial has very low overhead, so we'll just
        # subclass it to create our "do more work in the next bounce"
        # wrapper. Since this class is fairly well hidden, we can use
        # its type as a sentinel value to detect whether each bounce
        # of the trampoline should be rebounced or returned.
        pass


    def tco_trampoline(work, *args, **kwds):
        work = work(*args, **kwds)
        while _type(work) is TailCall:
            work = work()
        return work


    class FunctionTrampoline(partial):
        """
        A tail-call trampoline wrapper for a function
        """

        def __new__(cls, fun):
            # note we don't check for _tco_disable here, because by the
            # time we get this far, the function has been compiled
            # expecting to have a trampoline under it, and it will return
            # TailCall objects. Without the trampoline, those would end up
            # in the normal return flow, and would break a bunch of
            # things.

            # if fun is already a wrapped tailcall, or it's a wrapped
            # trampoline, we'll unwrap it first.
            fun = _getattr(fun, "_tco_original", fun)

            part = partial.__new__(cls, tco_trampoline, fun)
            part._tco_original = fun
            part._tco_enable = True
            return part

        def __repr__(self):
            return "<trampoline function %s>" % self._tco_original.__name__


    class MethodTrampoline(partialmethod):
        """
        A tail-call trampoline wrapper for a method
        """

        def __init__(self, fun):
            # note we don't check for _tco_disable here, because by the
            # time we get this far, the function has been compiled
            # expecting to have a trampoline under it, and it will return
            # TailCall objects. Without the trampoline, those would end up
            # in the normal return flow, and would break a bunch of
            # things.

            # if fun is already a wrapped tailcall, or it's a wrapped
            # trampoline, we'll unwrap it first.
            fun = _getattr(fun, "_tco_original", fun)

            partialmethod.__init__(self, tco_trampoline, fun)
            self._tco_original = fun
            self._tco_enable = True

        def __repr__(self):
            return "<trampoline method %s>" % self._tco_original.__name__


    trampoline = FunctionTrampoline
    methodtrampoline = MethodTrampoline


    def tailcall(fun):
        if not _getattr(fun, "_tco_enable", False):
            return fun

        # if fun is already a wrapped tailcall, or it's a wrapped
        # trampoline, we'll unwrap it first.
        fun = _getattr(fun, "_tco_original", fun)

        tco_bounce = partial(TailCall, fun)
        tco_bounce._tco_original = fun

        return tco_bounce


    trampoline.__qualname__ = "sibilant.tco.trampoline"
    trampoline.__qualname__ = "sibilant.tco.methodtrampoline"
    tailcall.__qualname__ = "sibilant.tco.tailcall"

    return trampoline, methodtrampoline, tailcall


trampoline, methodtrampoline, tailcall = setup()
del setup


def tco_disable(fun):
    """
    Decorator to instruct the tailcall optimization to never tailcall
    bounce the given function.
    """

    fun._tco_enable = False

    return fun


#
# The end.
