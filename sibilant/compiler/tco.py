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


# from collections import OrderedDict
from functools import partial, wraps


__all__ = (
    "trampoline", "tailcall",
)


class TailCall():
    def __init__(self, work, *args, **kwds):
        self.work = work
        self.args = args
        self.kwds = kwds


def trampoline(fun):
    _tc = TailCall

    @wraps(fun)
    def tco_trampoline(*args, **kwds):
        # TODO: should we gather the call frames that bounce out
        # so that in the event of a real exception we can produce
        # a more meaningful traceback?

        # frames = OrderedDict()
        work = fun

        while True:
            work = work(*args, **kwds)
            if type(work) is _tc:
                args = work.args
                kwds = work.kwds
                work = work.work
                continue
            return work

    tco_trampoline._tco_original = fun
    return tco_trampoline


def tailcall(fun):
    # if fun is already a wrapped tailcall, or it's a wrapped
    # trampoline, we'll unwrap it first.
    fun = getattr(fun, "_tco_original", fun)

    tco_bounce = partial(TailCall, fun)
    tco_bounce._tco_original = fun

    return tco_bounce


#
# The end.
