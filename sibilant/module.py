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


def module(name, sib_ast, positions=None, builtins=None, defaults=None):
    mod = ModuleType(name)

    if defaults:
        mod.__dict__.update(defaults)

    positions = positions or dict()

    eval_k(lambda v: None, sib_ast, positions, module)
    return module


def module_from_parser(name, parser_gen):
    return module(name, compose(parser_gen))


def module_from_stream(name, stream):
    return module(name, compose_from_stream(stream))


def module_from_str(name, src_str):
    return module(name, compose_from_str(src_str))


#
# The end.
