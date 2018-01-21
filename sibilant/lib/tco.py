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

    from sys import _xoptions
    from functools import partial

    if _xoptions.get("sibilant.ctco", "True") == "True":
        try:
            from .ctco import trampoline, tailcall
            return trampoline, tailcall

        except ImportError:
            # we wanted to load the ctco stuff, but we couldn't, so we
            # will fall back to the pythonic implementation
            pass


    _getattr = getattr


    class TailCall(partial):
        # invocation of partial has very low overhead, so we'll just
        # subclass it to create our "do more work in the next bounce"
        # wrapper. Since this class is fairly well hidden, we can use
        # its type as a sentinel value to detect whether each bounce
        # of the trampoline should be rebounced or returned.
        pass


    def tco_trampoline(work, *args, **kwds):
        # this is the workhorse function, the trampoline loop
        # itself. The sentinel signal that the trampoline should keep
        # running is that the work function returned a TailCall
        # instance. So long as the work is a TailCall, keep bouncing.

        work = work(*args, **kwds)
        while work.__class__ is TailCall:
            work = work()
        return work


    class Trampoline(partial):

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

            part.__name__ = fun.__name__
            part.__doc__ = fun.__doc__
            part.__qualname__ = fun.__qualname__

            return part


    Trampoline.__qualname__ = "Trampoline"


    class FunctionTrampoline(Trampoline):
        """
        A tail-call trampoline wrapper for a function
        """

        def __get__(self, inst, owner):
            return self if inst is None else \
                MethodTrampoline(self._tco_original.__get__(inst, owner))


        def __repr__(self):
            return "<trampoline function %s at 0x%x>" % \
                (self.__name__, id(self))


    FunctionTrampoline.__qualname__ = "FunctionTrampoline"


    class MethodTrampoline(Trampoline):
        """
        A tail-call trampoline wrapper for a method
        """

        def __repr__(self):
            return "<trampoline bound method %s of %r>" % \
                (self.__qualname__, self._tco_original.__self__)


    MethodTrampoline.__qualname__ = "MethodTrampoline"


    def tailcall(fun):
        # invocations of this function are what get injected at
        # compile-time to allow a function to become tail call
        # optimized. This simply defers the execution of a function so
        # that it may be invoked from the trampoline frame instead.

        if not _getattr(fun, "_tco_enable", False):
            return fun

        # if fun is already a wrapped tailcall, or it's a wrapped
        # trampoline, we'll unwrap it first.
        fun = _getattr(fun, "_tco_original", fun)

        tco_bounce = partial(TailCall, fun)

        return tco_bounce


    tailcall.__qualname__ = "sibilant.compiler.tco.tailcall"

    return FunctionTrampoline, tailcall


trampoline, tailcall = setup()
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
