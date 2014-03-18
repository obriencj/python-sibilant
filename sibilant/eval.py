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
Evaluation for Sibilant's continuation-passing style trampoline

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


from functools import partial


__all__ = ( "TrampolineCall", "ContinuationCall",
            "bounce", "bounce_exit",
            "evaluate", "evaluate_and_return" )


class TrampolineCall(BaseException):
    """
    Signals evaluate to bounce the call stack off and continue from
    the current continuation.
    """

    pass


class ContinuationCall(TrampolineCall):
    """
    A TrampolineCall triggered by an invocation of call/cc
    """

    pass


def bounce(k_cont, *value):
    """
    forces a trampoline bounce, and continues with k_cont(*value)
    """

    k = partial(k_cont, *value) if value else k_cont
    raise TrampolineCall(k).with_stacktrace(None)


def bounce_exit(value):
    """
    forces a trampoline bounce, continues with an immediate return of
    `value`. This effectively terminates the continuation-passing
    program and causes the nearest-level `evaluate` to return `value`
    """

    raise TrampolineCall(lambda: value).with_stacktrace(None)


def evaluate_and_return(expr, *args):
    """
    evaluate `expr` with a continuation that simply collects the
    results and then returns them from this function.
    """

    return evaluate(expr, bounce_exit, *args)


def evaluate(sfunc, k_cont, *args):
    """
    Enters into the evaluation mode and calls `sfunc` with the
    continuation function `k_cont` and any additional optional
    arguments.

    Parameters
    ----------
    sfunc : `function`
      the sibilant expression to call
    k_cont : `function(value)`
      the continuation to pass to the expression

    Returns
    -------
    value
      the result of `k_cont` when evaluation has completed
    """

    work = partial(sfunc, k, *args)
    while True:
        try:
            result = work()
        except TrampolineCall as tc:
            work = tc.args[0]
        else:
            return result


#
# The end.
