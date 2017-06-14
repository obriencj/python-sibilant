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


def eval_k(k, sib_ast, positions, module):
    # Here we are. The fun part.  This is where we convert the cons
    # cell sib_ast into a series of python expression ASTs which need
    # to chain recursively back to eval_k, since this is an
    # incremental compiler. Each expression changes how further
    # expressions may be interpreted!


    pass


def _trampoline(k, work, *args):
    # user-registered exception handlers
    handlers = nil

    work = partial(work, k, *args)
    while True:
        try:
            result = work()

        except TrampolineCall as tc:
            work = tc.k

        except PushHandler as push_h:
            handlers = cons(push_h.handlers, handlers)
            work = push_h.cont

        except ResetHandler as rest_h:
            handlers = rest_h.handlers
            work = rest_h.cont

        except GatherCallCC as gccc:
            work = _compose_call_cc(gccc.k, gccc.cont, handlers)

        except BaseException as exc:
            if handlers is nil:
                raise
            else:
                work, handlers = _trigger_exc(exc, handlers)

        else:
            return result


def _call_cc_k(k, work):
    """
    The call/cc builtin. k is the continuation of the call/cc
    invokation, work is the lambda which should accept a single
    argument, being the reified cc.

    Requires an outer _trampoline
    """

    raise GatherCallCC(k, work)


def _compose_call_cc(k, work, handlers):
    """
    handles call/cc reification in our _trampoline.

    returns a trampoline bounce which will call the work function with
    the call/cc's k and an additional parameter which is our reified
    cc -- being simply a function that will trigger a _trampoline
    event to reset the exception handlers to their current state and
    to bounce to the call/cc's k with a supplied value.
    """

    def cc(_k, value):
        # note that the continuation of the invocation of the CC is
        # discarded! return never returns ;-)
        raise ResetHandler(partial(k, value), handlers)
    return partial(work, k, cc)


def _trigger_exc(exc, handlers):
    """
    locate the work function from among handlers for exc, and return
    it along with the handlers list with all unmatched handlers popped
    off
    """

    while handlers is not nil:
        h, handlers = handlers
        # TODO
        pass


#
# The end.
