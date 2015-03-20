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
Continuation passing support for Sibilant

:author: Christopher O'Brien  <obriencj@gmail.com>
:license: LGPL v.3
"""


__all__ = (
    "TrampolineCall", "ContinuationCall",
    "bounce", "bounce_exit",
    "trampoline", "trampoline_and_return",
    "k_style", "k_style_of", "k_adapt",
    "k_wrap", "k_unwrap",
    "k_call", "de_k_call", )


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


def is_k_style(func):
    return getattr(func, "__k_style", False)


def k_style(func):
    """
    Decorator to indicate that a function is written in the k-style
    for use by `k_call`.
    """

    func.__k_style = True
    return func


def k_style_of(non_k_func):
    """
    Decorator to indicate that a function is written in the k-style
    and is a k-style alternative to an existing non-k-style function.
    """

    def decorator(k_func):
        non_k_func.__k_adapt = k_func
        k_func.__k_style = True
        return k_func
    return decorator


def k_adapt(wrapped=None):
    """
    Decorator to create a simple k-style wrapper for a non-k-style
    function, and to mark it as supported for use by `k_call`. The
    wrapper will be attached to `func.__k_adapt`
    """

    def decorator(func):
        func.__k_adapt = wrapped if wrapped else k_wrap(func)
        return func

    return decorator


def k_wrap(func):
    """
    Wrap a function to accept a firt-argument continuation, and mark
    it as being k-style. If the function is already k-style, it's
    returned unaltered. If the function has a k-style adaptor, the
    adaptor will be returned.
    """

    if func is None:
        return None

    elif getattr(func, "__k_style", False):
        return func

    else:
        wrapped = getattr(func, "__k_adapt", None)
        if not wrapped:
            def wrapped(k, *p, **kw):
                return k(func(*p, **kw))
            wrapped.__k_style = True
            wraps(wrapped, func)
        return wrapped


def _return_v(v):
    """
    simple continuation that uses python's in-built return to pass the
    value back to the original caller.
    """
    return v


def k_unwrap(k_func):
    """
    Wrap a k-style function to return, and to no longer accept
    continuation k as the firt parameter.
    """

    return partial(k_func, _return_v)


@k_style
def k_call(k, func, *args, **params):
    """
    Call a function which may or may-not be written k-style. If it is
    decorated as a k-style function, it will be called. If it has an
    adaptation that is k-style, the adaptation will be called.
    Otherwise, the function will be called and the result will be
    passed to the continuation k.
    """

    if getattr(func, "__k_style", False):
        return func(k, *args, **params)
    else:
        k_style_func = getattr(func, "__k_adapt", None)
        if k_style_func is None:
            return k(func(*args, **params))
        else:
            return k_style_func(k, *args, **params)


@k_adapt(k_call)
def de_k_call(func, *args, **params):
    """
    Calls a k-style function with a continuation that collects and
    returns the result here. Note that a continuation captured from
    within this call chain will not be able to be re-called, as the
    continuation chain is broken in order to adapt to the normal
    Pythonic return style. Use this at your own risk.
    """

    k_style_func = getattr(func, "__k_style", None)
    if k_style_func is None:
        return func(*args, **params)
    else:
        return k_style_func(_return_v, *args, **params)



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


def trampoline_and_return(expr, *args):
    """
    evaluate `expr` with a continuation that simply collects the
    results and then returns them from this function.
    """

    return trampoline(expr, bounce_exit, *args)


def trampoline(sfunc, k_cont, *args):
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
