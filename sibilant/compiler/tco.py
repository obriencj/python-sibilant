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


from functools import wraps
from traceback import clear_frames


__all__ = (
    "TailCall", "trampoline", "tailcall",
)


class TailCall(BaseException):
    def __init__(self, work, args, kwds):
        self.work = work
        self.args = args
        self.kwds = kwds


def trampoline(fun):
    _cf = clear_frames

    @wraps(fun)
    def tco_trampoline(*args, **kwds):
        # TODO: should we gather the call frames that bounce out
        # so that in the event of a real exception we can produce
        # a more meaningful traceback?

        work = fun

        while True:
            try:
                work = work(*args, **kwds)
            except TailCall as tc:
                work = tc.work
                args = tc.args
                kwds = tc.kwds
                _cf(tc.__traceback__)
            else:
                break

        return work

    tco_trampoline._tco_trampoline = fun
    return tco_trampoline


def tailcall(fun):
    work = getattr(fun, "_tco_trampoline", fun)

    @wraps(work)
    def tco_bounce(*args, **kwds):
        raise TailCall(work, args, kwds).with_traceback(None)

    return tco_bounce


#
# The end.
