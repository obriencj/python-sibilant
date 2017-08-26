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
The built-in compile-time special forms
"""


from .. import (
    symbol, is_symbol, keyword, is_keyword,
    nil, is_nil, cons, cdr, is_pair, is_proper,
)

from . import is_macro, gather_formals


__all__ = []


_symbol_nil = symbol("nil")

_symbol_doc = symbol("doc")
_symbol_attr = symbol("attr")
_symbol_set_attr = symbol("set-attr")
_symbol_setq = symbol("setq")
_symbol_global = symbol("global")
_symbol_define = symbol("define")
_symbol_define_global = symbol("define-global")
_symbol_define_local = symbol("define-local")
_symbol_defmacro = symbol("defmacro")
_symbol_quote = symbol("quote")
_symbol_quasiquote = symbol("quasiquote")
_symbol_unquote = symbol("unquote")
_symbol_splice = symbol("unquote-splicing")
_symbol_begin = symbol("begin")
_symbol_cond = symbol("cond")
_symbol_lambda = symbol("lambda")
_symbol_function = symbol("function")
_symbol_with = symbol("with")
_symbol_let = symbol("let")
_symbol_while = symbol("while")
_symbol_try = symbol("try")

_symbol_macroexpand_1 = symbol("macroexpand-1")

_keyword_else = keyword("else")
_keyword_as = keyword("as")
_keyword_finally = keyword("finally")


def special():
    from . import Special
    glbls = globals()

    def special(namesym, *aliases):
        name = str(namesym)

        def deco(compilefn):
            compilefn.__name__ = name
            inst = Special(str(namesym), compilefn)

            __all__.append(name)
            glbls[name] = inst

            for alias in aliases:
                alias = str(alias)
                __all__.append(alias)
                glbls[alias] = inst

        return deco

    return special

special = special()


# --- special forms ---


def _helper_binding(code, source, required=True):
    if not is_proper(source):
        raise code.error("binding must be (SYM EXPR) or (EXPR as: SYM)",
                         source)

    sc = source.count()
    if sc == 3:
        expr, (_as, (sym, _rest)) = source
        if not (_as is _keyword_as and is_symbol(sym)):
            raise code.error("binding must be (SYM EXPR) or (EXPR as: SYM)",
                             source)

    elif sc == 2:
        sym, (expr, _rest) = source
        if not is_symbol(sym):
            raise code.error("binding must be (SYM EXPR) or (EXPR as: SYM)",
                             source)

    elif sc == 1 and not required:
        expr, _rest = source
        sym = required

    else:
        raise code.error("binding must be (SYM EXPR) or (EXPR as: SYM)",
                         source)

    return sym, expr


def _helper_keyword(code, kwd):
    """
    Pushes a the pseudo ops necessary to put a keyword on the stack
    """

    code.pseudop_get_var("keyword")
    code.pseudop_const(str(kwd))
    code.pseudop_call(1)


def _helper_symbol(code, sym):
    """
    Pushes a the pseudo ops necessary to put a symbol on the stack
    """

    code.pseudop_get_var("symbol")
    code.pseudop_const(str(sym))
    code.pseudop_call(1)


@special(_symbol_doc)
def _special_doc(code, source, tc=False):
    """
    (doc STR STR...)
    joins STR together and sets it as the docstr for the parent scope
    """

    called_by, rest = source

    code.set_doc(" ".join(d.strip() for d in map(str, rest.unpack())))

    # doc special expression evaluates to None
    code.pseudop_const(None)

    return None


@special(_symbol_attr)
def _special_get_attr(code, source, tc=False):
    try:
        called_by, (obj, (member, rest)) = source
    except ValueError:
        raise code.error("too few arguments to attr", source)

    if not is_nil(rest):
        raise code.error("too many arguments to attr", source)

    code.pseudop_position_of(source)
    code.add_expression(obj)
    code.pseudop_getattr(str(member))

    # no further transformations
    return None


@special(_symbol_set_attr)
def _special_set_attr(code, source, tc=False):
    try:
        called_by, (obj, (member, (value, rest))) = source
    except ValueError:
        raise code.error("too few arguments to set-attr", source)

    if not is_nil(rest):
        raise code.error("too many arguments to set-attr", source)

    code.add_expression(obj)
    code.add_expression(value)
    code.pseudop_rot_two()
    code.pseudop_setattr(str(member))

    # make setf calls evaluate to None
    code.pseudop_const(None)

    # no further transformations
    return None


@special(_symbol_quote)
def _special_quote(code, source, tc=False):
    """
    Special form for quote
    """

    called_by, body = source

    if not body:
        code.error("Too fuew arguments to quote %s" % source, source)

    body, _rest = body

    if _rest:
        code.error("Too many arguments to quote %s" % source, source)

    code.pseudop_position_of(source)
    _helper_quote(code, body)

    # no additional transform needed
    return None


def _helper_quote(code, body, tc=False):
    if body is nil:
        code.pseudop_get_var("nil")

    elif is_keyword(body):
        _helper_keyword(code, body)

    elif is_symbol(body):
        _helper_symbol(code, body)

    elif is_pair(body):
        if is_proper(body):
            code.pseudop_get_var("make-proper")
        else:
            code.pseudop_get_var("cons")

        for index, expr in enumerate(body.unpack(), 1):
            _helper_quote(code, expr)
        code.pseudop_call(index)

    else:
        code.pseudop_const(body)


@special(_symbol_unquote)
def _special_unquote(code, source, tc=False):
    """
    (quote EXPR)

    Returns the raw form of EXPR, without evaluating it

    'EXPR

    Same as (quote EXPR)
    """

    raise code.error("unquote outside of quasiquote", source)


@special(_symbol_splice)
def _special_splice(code, source, tc=False):
    raise code.error("splice outside of quasiquote", source)


@special(_symbol_quasiquote)
def _special_quasiquote(code, source, tc=False):
    """
    (quasiquote EXPR)
    Special form for quasiquote

    `EXPR
    Same as (quasiquote EXPR)
    """

    called_by, (body, rest) = source

    if rest:
        raise code.error("Too many arguments to quasiquote", source)

    code.pseudop_position_of(source)
    _helper_quasiquote(code, body)

    return None


def _helper_quasiquote(code, marked, level=0):
    # print("helper_quasiquote level:", level)
    # print("marked:", marked)

    if marked is nil or marked is _symbol_nil:
        code.pseudop_get_var("nil")
        return

    elif is_keyword(marked):
        _helper_keyword(code, marked)
        return

    elif is_symbol(marked):
        _helper_symbol(code, marked)
        return

    elif is_pair(marked):
        if is_proper(marked):
            head, tail = marked

            if head is _symbol_unquote:
                tail, _rest = tail
                if level == 0:
                    return code.add_expression(tail)
                else:
                    code.pseudop_get_var("make-proper")
                    _helper_symbol(code, head)
                    _helper_quasiquote(code, tail, level - 1)
                    code.pseudop_call(2)
                    return

            elif head is _symbol_splice:
                tail, _rest = tail
                if level == 0:
                    code.pseudop_get_var("make-proper")
                    code.pseudop_get_var("to-tuple")
                    code.add_expression(tail)
                    code.pseudop_call(1)
                    code.pseudop_call_var(0)
                    return
                else:
                    code.pseudop_get_var("make-proper")
                    _helper_symbol(code, head)
                    _helper_quasiquote(code, tail, level - 1)
                    code.pseudop_call(2)
                    return

            elif head is _symbol_quasiquote:
                tail, _rest = tail
                code.pseudop_get_var("make-proper")
                _helper_symbol(code, head)
                _helper_quasiquote(code, tail, level + 1)
                code.pseudop_call(2)
                return

            code.pseudop_get_var("make-proper")
        else:
            code.pseudop_get_var("cons")

        coll_tup = 0  # the count of collected tuples
        curr_tup = 0  # the size of the current tuple

        for expr in marked.unpack():
            if expr is nil or expr is _symbol_nil:
                curr_tup += 1
                code.pseudop_get_var("nil")
                continue

            elif is_keyword(expr):
                _helper_keyword(code, expr)
                curr_tup += 1
                continue

            elif is_symbol(expr):
                _helper_symbol(code, expr)
                curr_tup += 1
                continue

            elif is_pair(expr):
                if is_proper(expr):
                    head, tail = expr

                    if head is _symbol_quasiquote:
                        tail, _rest = tail
                        code.pseudop_get_var("make-proper")
                        _helper_symbol(code, head)
                        _helper_quasiquote(code, tail, level + 1)
                        code.pseudop_call(2)
                        curr_tup += 1
                        continue

                    elif head is _symbol_unquote:
                        u_expr, tail = tail

                        # print("unquote level:", level)
                        # print("expr:", u_expr)

                        if level == 0:
                            # either not proper or not splice
                            code.add_expression(u_expr)
                            curr_tup += 1
                            continue

                        else:
                            # not level 0, recurse with one less level
                            code.pseudop_get_var("make-proper")
                            _helper_symbol(code, head)
                            _helper_quasiquote(code, u_expr, level - 1)
                            code.pseudop_call(2)
                            curr_tup += 1
                            continue

                    elif head is _symbol_splice:
                        u_expr, tail = tail

                        if level == 0:
                            if curr_tup:
                                code.pseudop_build_tuple(curr_tup)
                                curr_tup = 0
                                coll_tup += 1

                            code.pseudop_get_var("to-tuple")
                            code.add_expression(u_expr)
                            code.pseudop_call(1)
                            coll_tup += 1
                            continue

                        else:
                            code.pseudop_get_var("make-proper")
                            _helper_symbol(code, head)
                            _helper_quasiquote(code, u_expr, level - 1)
                            code.pseudop_call(2)
                            curr_tup += 1
                            continue

                # a pair, but not an unquote
                _helper_quasiquote(code, expr, level)
                curr_tup += 1
                continue

            else:
                # not a nil, symbol, or pair, so evaluates to its
                # own code as a constant
                code.pseudop_const(expr)
                curr_tup += 1
                continue

        # after iterating through the expressions of marked.unpack
        # we can check if we've accumulated anything.
        if curr_tup:
            code.pseudop_build_tuple(curr_tup)
            curr_tup = 0
            coll_tup += 1

        assert coll_tup, "no members accumulated"
        code.pseudop_build_tuple_unpack(coll_tup)
        code.pseudop_call_var(0)

    else:
        # some... other thing.
        code.pseudop_const(marked)


@special(_symbol_begin)
def _special_begin(code, source, tc=False):
    """
    (begin EXPR EXPR...)

    Evaluates each EXPR in turn. Returns the last result only.

    (begin)

    Evaluates to None
    """

    called_by, body = source

    _helper_begin(code, body, tc=tc)

    # no additional transform needed
    return None


def _helper_begin(code, body, tc=False):
    code.pseudop_position_of(body)

    if not body:
        # because all things are expressions, an empty begin still
        # needs to have a return value.
        code.pseudop_const(None)

    else:
        # a non-empty body needs to evaluate all of its child
        # expressions, but only keep the last one on the stack.
        while True:
            expr, body = body
            if body is nil:
                code.add_expression(expr, tc=tc)
                break
            else:
                code.add_expression(expr, False)
                code.pseudop_pop()

    return None


@special(_symbol_with)
def _special_with(code, source, tc=False):
    """
    (with (BINDING EXPRESSION) BODY)

    Enters the context manager from EXPRESSION, binding it to BINDING,
    and evaluates the expressions of BODY. The final value of BODY is
    the result.
    """

    called_by, (args, body) = source

    binding, expr = _helper_binding(code, args)

    binding = str(binding)
    code.declare_var(str(binding))

    storage = code.gen_sym()
    code.declare_var(storage)

    label_cleanup = code.gen_label()

    code.add_expression(expr)
    code.pseudop_setup_with(label_cleanup)
    code.pseudop_faux_push(4)
    code.pseudop_set_var(binding)

    _helper_begin(code, body, tc)
    code.pseudop_set_var(storage)

    code.pseudop_pop_block()
    code.pseudop_const(None)
    code.pseudop_faux_pop()

    code.pseudop_label(label_cleanup)
    code.pseudop_with_cleanup_start()
    code.pseudop_with_cleanup_finish()
    code.pseudop_end_finally()

    code.pseudop_get_var(storage)
    code.pseudop_del_var(storage)
    code.pseudop_faux_pop(3)

    return None


@special(_symbol_lambda)
def _special_lambda(code, source, tc=False):
    """
    (lambda (FORMAL...) BODY...)

    Creates an anonymous function taking FORMAL arguments, and executing
    the BODY expressions in order. The result of the final expression is
    returned.
    """

    called_by, (args, body) = source

    code.pseudop_position_of(source)

    _helper_function(code, "<lambda>", args, body,
                     declared_at=code.position_of(source))

    # no additional transform needed
    return None


@special(_symbol_function)
def _special_function(code, source, tc=False):
    """
    (function NAME (FORMAL...) BODY...)

    Creates a function with a binding to itself as NAME, taking FORMAL
    arguments, and executing the BODY expressions in order. The result
    of the final expression is returned.
    """

    called_by, (namesym, cl) = source
    args, body = cl

    # todo create the function inside of a closure that has a
    # single local cell, which is the new function's name. this
    # will give the function the ability to reference its cell via
    # that cell.

    name = str(namesym)
    declared_at = code.position_of(source)

    code.pseudop_position_of(source)

    kid = code.child_context(declared_at=declared_at)
    with kid as subc:
        subc.declare_var(name)
        _helper_function(subc, name, args, body, declared_at=declared_at)
        subc.pseudop_dup()
        subc.pseudop_set_var(name)
        subc.pseudop_return()
        kid_code = subc.complete()

    code.pseudop_lambda(kid_code, 0)
    code.pseudop_call(0)

    # no additional transform needed
    return None


@special(_symbol_let)
def _special_let(code, source, tc=False):
    """
    (let ((BINDING EXPR) ...) BODY...)

    Creates a lexical scope where each BINDING is assigned the value from
    evaluating EXPR, then executes the BODY expressions in order. The
    result of the final expression is returned.
    """

    called_by, (bindings, body) = source

    args = []
    vals = []
    for arg in bindings.unpack():
        name, val = _helper_binding(code, arg)
        args.append(name)
        vals.append(val)

    if args:
        args = cons(*args, nil)
    else:
        args = nil

    code.pseudop_position_of(source)

    _helper_function(code, "<let>", args, body,
                     declared_at=code.position_of(source))

    code.helper_tailcall_tos(tc)

    for val in vals:
        code.add_expression(val)

    code.pseudop_call(len(vals))

    # no additional transform needed
    return None


def _helper_function(code, name, args, body, declared_at=None):

    tco = code.tco_enabled

    if not (is_symbol(name) or isinstance(name, str)):
        msg = "function names must be symbol or str, not %r" % name
        raise code.error(msg, declared_at)

    formals = gather_formals(args, code.position_of(args) or declared_at)
    pos, keys, defaults, star, starstar = formals
    proper = is_proper(args)

    argnames = list(map(str, pos))

    if keys:
        argnames.extend(map(str, keys))

    if star:
        varargs = True
        argnames.append(str(star))
    else:
        varargs = False

    if starstar:
        varkeywords = True
        argnames.append(str(starstar))
    else:
        varkeywords = False

    if declared_at is None:
        declared_at = code.position_of(body)

    for expr in defaults:
        code.add_expression(expr)

    kid = code.child_context(args=argnames,
                             varargs=varargs,
                             varkeywords=varkeywords,
                             name=name,
                             declared_at=declared_at,
                             proper_varargs=proper,
                             tco_enabled=tco)

    with kid as subc:
        _helper_begin(subc, body, tco)
        subc.pseudop_return()
        code.pseudop_lambda(subc.complete(), len(defaults))

        if tco and subc.tailcalls > 0:
            code.pseudop_get_var("trampoline")
            code.pseudop_rot_two()
            code.pseudop_call(1)


@special(_symbol_while)
def _special_while(code, source, tc=False):
    """
    (while TEST_EXPR BODY...)

    Evaluates TEST_EXPR. If the result is truthful, evaluates the BODY
    expressions in order, then repeats until the evaluation of
    TEST_EXPR is not truthful. The result is the last successful BODY
    expression, or None if BODY was never executed.
    """

    called_by, (test, body) = source

    top = code.gen_label()
    done = code.gen_label()

    # pre-populate our return value
    code.pseudop_const(None)

    code.pseudop_label(top)

    code.add_expression(test)
    code.pseudop_pop_jump_if_false(done)

    # throw away previous value in favor of evaluating the body
    code.pseudop_pop()
    _helper_begin(code, body)

    code.pseudop_jump(top)
    code.pseudop_label(done)

    # no additional transform needed
    return None


@special(_symbol_try)
def _special_try(code, source, tc=False):
    """
    (try EXPR
      (except: EXCEPTION_TYPE EXC_BODY...)
      ...)

    Attempts evaluation of EXPR. If an exception is raised, and it
    matches the EXCEPTION_TYPE expression, then evaluate the
    expressions of EXC_BODY in order and return the last result.
    Multiple exception matches can be specified. If no EXCEPTION_TYPE
    matches, the exception is propagated upwards and this function does
    not return.

    (try EXPR
      ((BINDING EXCEPTION_TYPE) EXC_BODY...)
      ...)

    As above, but binds the exception to BINDING before evaluating
    EXC_BODY.

    (try EXPR
      EXC_HANDLERS...
      (else: BODY...))

    As above, but if no exception was raised by EXPR, the expressions of
    BODY are evaluated in order and the last value is returned. Note that
    if an exception is raised during BODY eval, it is not caught.

    (try EXPR
      EXC_HANDLERS...
      (finally: BODY...))

    As above, but the expressions of BODY are evaluated in order
    whether an exception was or was not raised or caught. If an
    exception was raised but not matched, then once BODY is completed,
    the exception will be propagated upwards and this function does
    not return. Otherwise, the final value of BODY is returned.
    """

    _helper_special_try(code, source, tc)

    return None


def _helper_special_try(code, source, tc=False):

    called_by, (try_expr, catches) = source

    storage = code.gen_sym()
    code.declare_var(storage)

    the_end = code.gen_label()

    has_finally = False
    has_else = False

    normal_catches = []

    code.pseudop_debug("top of _helper_special_try")

    # first, filter our catches down into normal exception
    # matches, finally, and else
    for ca in catches.unpack():
        if not ca:
            raise code.error("clause with no body in try", ca)

        ex, act = ca
        if is_proper(ex):
            ex_binding, ex_expr = _helper_binding(code, ex, None)

            ex = cons([ex_binding, ex_expr], act)
            code.dup_position_of(ca, ex)
            normal_catches.append(ex)

        elif ex is _keyword_finally:
            if has_finally:
                raise code.error("duplicate finally: clause in try", ca)

            has_finally = True
            act_finally = act
            label_finally = code.gen_label()

        elif ex is _keyword_else:
            if has_else:
                raise code.error("duplicate else: clause in try", ca)
            has_else = True
            act_else = act
            label_else = code.gen_label()

        else:
            raise code.error("missing keyword label in else/finally", ca)

    label_next = code.gen_label()

    if has_finally:
        # setup that finally block if we need one
        code.pseudop_setup_finally(label_finally)

    # here's our actual try block
    code.pseudop_setup_except(label_next)
    code.add_expression(try_expr)
    code.pseudop_debug("after try expression")

    if has_else:
        # the result of the expression is thrown away if we have
        # an else clause. This should be redundant, due to the
        # following pop_block, but let's be tidy
        code.pseudop_pop()
        code.pseudop_pop_block()
        code.pseudop_jump_forward(label_else)

    else:
        # but if there isn't an else clause, then the result of
        # the expression is the real deal. If there was a finally,
        # that will be jumped to.
        code.pseudop_set_var(storage)
        code.pseudop_pop_block()
        code.pseudop_jump_forward(the_end)

    code.pseudop_debug("before handlers")

    # TOS exception type, TOS-1 exception, TOS-2 backtrace
    for ca in normal_catches:
        (key, match), act = ca

        # each attempt to match the exception should have us at
        # the TOS,TOS-1,TOS-2 setup
        # PLUS the 3 from an exception block
        code.pseudop_faux_push(7)

        code.pseudop_position_of(ca)
        code.pseudop_label(label_next)

        label_next = code.gen_label()

        code.pseudop_debug("beginning of declared handler")

        if key:
            # The exception is intended to be bound to a local
            # variable. To achieve that, we're going to set up
            # a lambda with that single binding, and stuff out
            # handler code inside of it.

            key = str(key)
            code.declare_var(key)

            cleanup = code.gen_label()

            # check if we match. If not, jump to the next catch
            # attempt
            code.pseudop_dup()
            code.add_expression(match)
            code.pseudop_compare_exception()
            code.pseudop_pop_jump_if_false(label_next)

            # okay, we've matched, so we need to bind the
            # exception instance to the key and clear the
            # other exception members off of the stack
            code.pseudop_pop()         # pops the dup'd exc type
            code.pseudop_set_var(key)  # binds the exc instance
            code.pseudop_pop()         # pops the traceback

            # wrap our handler code in a finally block, so
            # that we can delete the key from the local
            # namespace afterwards
            code.pseudop_setup_finally(cleanup)

            # handle the exception, store the result
            _helper_begin(code, act, tc and not has_finally)
            code.pseudop_set_var(storage)
            code.pseudop_pop_block()
            code.pseudop_pop_except()

            # no idea why this None needs to be on the stack, but it
            # does.
            code.pseudop_const(None)
            code.pseudop_label(cleanup)

            # kill off that exception binding
            code.pseudop_const(None)
            code.pseudop_set_var(key)
            code.pseudop_del_var(key)

            # end_finally pops our cleanup block and
            # finishes returning
            code.pseudop_end_finally()
            code.pseudop_jump_forward(the_end)

        else:
            # this is an exception match without a binding
            code.pseudop_dup()
            code.add_expression(match)
            code.pseudop_compare_exception()
            code.pseudop_pop_jump_if_false(label_next)

            # Now let's throw all that stuff away.
            code.pseudop_pop()
            code.pseudop_pop()
            code.pseudop_pop()

            _helper_begin(code, act, tc and not has_finally)
            code.pseudop_set_var(storage)

            code.pseudop_pop_except()
            code.pseudop_jump_forward(the_end)

    # after all the attempts at trying to match the exception, we
    # land here. This is the catch-all fallback, that will
    # re-raise the exception.
    code.pseudop_label(label_next)
    code.pseudop_faux_push(4)
    code.pseudop_debug("restored stack in fall-through")
    code.pseudop_faux_pop(3)
    code.pseudop_end_finally()
    code.pseudop_debug("fall-through exception")

    if has_else:
        # okay, we've arrived at the else handler. Let's run it,
        # and store that value
        code.pseudop_label(label_else)
        code.pseudop_debug("start of else handler")
        _helper_begin(code, act_else, tc and not has_finally)

        # if there is a finally registered, this will trigger it
        # to run (and possibly overwrite the return value)
        code.pseudop_set_var(storage)
        code.pseudop_debug("end of else handler")

    if has_finally:
        # here's the actual handling of the finally event. This will
        # get jumped to from the pop_blocks above, if the finally
        # block was registered.
        code.pseudop_label(the_end)
        code.pseudop_pop_block()

        # run the handler and store the result
        code.pseudop_const(None)
        code.pseudop_label(label_finally)

        _helper_begin(code, act_finally, tc)
        code.pseudop_set_var(storage)

        # and close off the finally block
        code.pseudop_debug("closing off finally")
        code.pseudop_end_finally()

    else:
        code.pseudop_label(the_end)

    code.pseudop_get_var(storage)

    # cleanup our storage var
    code.pseudop_const(None)
    code.pseudop_set_var(storage)
    code.pseudop_del_var(storage)

    code.pseudop_debug("at the end")

    # no further transformations needed on the passed source code.
    return None


@special(_symbol_setq)
def _special_setq(code, source, tc=False):
    """
    (setq SYM EXPR)

    binds the result of evaluating EXPR to SYM)
    """

    called_by, (binding, body) = source

    if not is_symbol(binding):
        raise code.error("assignment must be by symbolic name",
                         cdr(source))

    value, rest = body
    if not is_nil(rest):
        raise code.error("extra values in assignment", source)

    if not is_pair(value):
        code.pseudop_position_of(body)

    code.add_expression(value)
    code.pseudop_set_var(str(binding))

    # set-var calls should evaluate to None
    code.pseudop_const(None)

    # no additional transform needed
    return None


@special(_symbol_global)
def _special_global(code, source, tc=False):

    called_by, (binding, rest) = source
    if not is_nil(rest):
        raise code.error("extra values in global lookup", source)

    code.pseudop_position_of(source)
    code.pseudop_get_global(str(binding))

    return None


@special(_symbol_define_global, _symbol_define)
def _special_define_global(code, source, tc=False):

    called_by, (binding, body) = source

    _helper_begin(code, body)

    if not is_symbol(binding):
        raise code.error("define-global with non-symbol binding", source)

    code.pseudop_position_of(source)
    code.pseudop_define_global(str(binding))

    # define expression evaluates to None
    code.pseudop_const(None)

    return None


@special(_symbol_define_local)
def _special_define_local(code, source, tc=False):

    called_by, (binding, body) = source

    _helper_begin(code, body)

    if not is_symbol(binding):
        raise code.error("define-local with non-symbol binding", source)

    code.pseudop_position_of(source)
    code.pseudop_define_local(str(binding))

    # define expression evaluates to None
    code.pseudop_const(None)

    return None


@special(_symbol_cond)
def _special_cond(code, source, tc=False):

    called_by, cl = source

    done = code.gen_label()
    label = code.gen_label()

    for test, body in cl.unpack():
        code.pseudop_label(label)
        label = code.gen_label()

        if test is _keyword_else:
            # print(repr(body))
            _helper_begin(code, body, tc)
            code.pseudop_jump_forward(done)
            break

        else:
            code.add_expression(test)
            code.pseudop_pop_jump_if_false(label)
            _helper_begin(code, body, tc)
            code.pseudop_jump_forward(done)

    else:
        # there was no else statement, so add a catch-all
        code.pseudop_label(label)
        code.pseudop_const(None)

    code.pseudop_label(done)

    return None


@special(_symbol_macroexpand_1)
def _special_macroexpand_1(code, source, tc=False):
    called_by, body = source

    if is_symbol(body):
        namesym = body
        args = nil

    elif is_proper(body):
        namesym, args = body

        if not is_symbol(namesym):
            msg = "cannot expand: %r" % namesym
            raise code.error(msg, source)

    else:
        msg = "invalid parameter to %s: %r" % (called_by, body)
        raise code.error(msg, source)

    found = code.find_compiled(namesym)
    if not is_macro(found):
        msg = "%s is not a macro: %r" % (namesym, found)
        raise code.error(msg, source)

    # TODO: emulate macro expansion better
    return _helper_quote(code, found.expand(*args))


# --- and finally clean up ---


__all__ = tuple(__all__)


#
# The end.
