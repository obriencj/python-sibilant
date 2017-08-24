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


__all__ = (
    "trampoline", "tailcall",
)


class TailCall(BaseException):
    def __init__(self, work, args, kwds):
        self.work = work
        self.args = args
        self.kwds = kwds


def trampoline(fun):
    @wraps(fun)
    def tco_trampoline(*args, **kwds):
        work = fun

        while True:
            try:
                return work(*args, **kwds)
            except TailCall as tc:
                work = tc.work
                args = tc.args
                kwds = tc.kwds

    tco_trampoline._tco_trampoline = fun
    return tco_trampoline


def tailcall(fun):
    work = getattr(fun, "_tco_trampoline", fun)
    @wraps(work)
    def tco_jump(*args, **kwds):
        raise TailCall(work, args, kwds).with_traceback(None)


#
# The end.
