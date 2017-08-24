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

        # frames = OrderedDict()
        work = fun

        while True:
            try:
                work = work(*args, **kwds)

            except TailCall as tailc:
                work = tailc.work
                args = tailc.args
                kwds = tailc.kwds

                # tb = tailc.__traceback__.tb_next
                # tc = tb.tb_frame.f_code

                # key = (tc.co_filename, tc.co_name, tb.tb_lineno)
                # frames[key] = frames.get(key, 0) + 1

                _cf(tailc.__traceback__)

            else:
                break

        return work

    tco_trampoline._tco_original = fun
    return tco_trampoline


def tailcall(fun):
    # if fun is already a wrapped tailcall, or it's a wrapped
    # trampoline, we'll unwrap it first.
    fun = getattr(fun, "_tco_original", fun)

    @wraps(fun)
    def tco_bounce(*args, **kwds):
        raise TailCall(fun, args, kwds).with_traceback(None)

    tco_bounce._tco_original = fun
    return tco_bounce


#
# The end.
