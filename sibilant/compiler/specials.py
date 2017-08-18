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
    symbol, is_symbol,
    keyword, is_keyword,
    nil, is_nil, cons, is_pair, is_proper,
)


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
_symbol_raise = symbol("raise")
_symbol_try = symbol("try")
_symbol_else = symbol("else")
_symbol_finally = symbol("finally")


def special():
    from . import Special
    glbls = globals()

    def special(namesym, *aliases):
        name = str(namesym)

        def deco(compilefn):
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
def _special_doc(code, source):
    called_by, rest = source

    code.set_doc(" ".join(d.strip() for d in map(str, rest.unpack())))

    # doc special expression evaluates to None
    code.pseudop_const(None)

    return None


@special(_symbol_attr)
def _special_get_attr(code, source):
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
def _special_set_attr(code, source):
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
def _special_quote(code, source):
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


def _helper_quote(code, body):
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
def _special_unquote(code, source):
    raise code.error("unquote outside of quasiquote", source)


@special(_symbol_splice)
def _special_splice(code, source):
    raise code.error("splice outside of quasiquote", source)


@special(_symbol_quasiquote)
def _special_quasiquote(code, source):
    """
    Special form for quasiquote
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
                    code.pseudop_call_varargs(0)
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
        code.pseudop_call_varargs(0)

    else:
        # some... other thing.
        code.pseudop_const(marked)


@special(_symbol_begin)
def _special_begin(code, source):
    """
    Special form for begin
    """

    called_by, body = source

    _helper_begin(code, body)

    # no additional transform needed
    return None


def _helper_begin(code, body):
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
            code.add_expression(expr)
            if body is nil:
                break
            else:
                code.pseudop_pop()

    return None


@special(_symbol_with)
def _special_with(code, source):
    """
    Special form for managed context via with
    """

    called_by, (args, body) = source

    if args.count() != 2:
        msg = "with context must be binding and expression," \
              " not %r" % args
        raise code.error(msg, cdr(source))

    binding, (expr, _rest) = args

    if not is_symbol(binding):
        msg = "binding must be a symbol, not %r" % binding
        raise code.error(msg, args)

    binding = str(binding)
    code.declare_var(str(binding))

    storage = code.gen_sym()
    code.declare_var(storage)

    label_cleanup = code.gen_label()

    code.add_expression(expr)
    code.pseudop_setup_with(label_cleanup)
    code.pseudop_faux_push(4)
    code.pseudop_set_var(binding)

    _helper_begin(code, body)
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
def _special_lambda(code, source):
    """
    Special form for lambda
    """

    called_by, (args, body) = source

    code.pseudop_position_of(source)

    _helper_function(code, "<lambda>", args, body,
                     declared_at=code.position_of(source))

    # no additional transform needed
    return None


@special(_symbol_function)
def _special_function(code, source):

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

    code.pseudop_lambda(kid_code)
    code.pseudop_call(0)

    # no additional transform needed
    return None


@special(_symbol_let)
def _special_let(code, source):

    called_by, (bindings, body) = source

    args = []
    vals = []
    for arg in bindings.unpack():
        name, val = arg.unpack()
        args.append(str(name))
        vals.append(val)

    code.pseudop_position_of(source)

    _helper_function(code, "<let>", args, body,
                     declared_at=code.position_of(source))

    for val in vals:
        code.add_expression(val)

    code.pseudop_call(len(vals))

    # no additional transform needed
    return None


def _helper_function(code, name, args, body, declared_at=None):
    if is_symbol(args):
        varargs = True
        args = [str(args)]

    elif is_pair(args):
        varargs = not is_proper(args)
        args = map(str, args.unpack())

    elif isinstance(args, (list, tuple)):
        varargs = False
        args = map(str, args)

    else:
        msg = "formals must be symbol or pair, not %r" % type(args)
        raise code.error(msg, cl)

    if declared_at is None:
        declared_at = code.position_of(body)

    kid = code.child_context(args=args, varargs=varargs,
                             name=name,
                             declared_at=declared_at)

    with kid as subc:
        _helper_begin(subc, body)
        subc.pseudop_return()
        kid_code = subc.complete()

    code.pseudop_lambda(kid_code)


@special(_symbol_while)
def _special_while(code, source):
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


@special(_symbol_raise)
def _special_raise(code, source):

    called_by, cl = source

    c = cl.count()
    if c > 3:
        msg = "too many arguments to raise %r" % cl
        raise code.error(msg, source)

    for rx in cl.unpack():
        code.add_expression(rx)

    code.pseudop_position_of(source)
    code.pseudop_raise(c)

    return None


@special(_symbol_try)
def _special_try(code, source):

    kid = code.child_context(name="<try>",
                             declared_at=code.position_of(source))

    with kid as subc:
        _helper_special_try(code, source)
        kid_code = subc.complete()

    code.pseudop_lambda(kid_code)
    code.pseudop_call(0)

    return None


def _helper_special_try(code, source):

    called_by, (expr, catches) = source

    the_end = code.gen_label()

    has_finally = False
    has_else = False

    normal_catches = []

    code.pseudop_debug("top of _helper_special_try")

    # first, filter our catches down into normal exception
    # matches, finally, and else
    for ca in catches.unpack():
        ex, act = ca

        if not act:
            raise code.error("clause with no body in try", ca)

        if ex is _symbol_finally:
            if has_finally:
                raise code.error("duplicate finally clause in try", ca)

            has_finally = True
            act_finally = act
            label_finally = code.gen_label()

        elif ex is _symbol_else:
            if has_else:
                raise code.error("duplicate else clause in try", ca)
            has_else = True
            act_else = act
            label_else = code.gen_label()

        else:
            # this is a normal catch, where ex is a thing to
            # match, and act is the body to execute if it matches.
            normal_catches.append(ca)

    label_next = code.gen_label()

    if has_finally:
        # setup that finally block if we need one
        code.pseudop_setup_finally(label_finally)

    # here's our actual try block
    code.pseudop_setup_except(label_next)
    code.add_expression(expr)
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
        code.pseudop_return()
        code.pseudop_pop_block()
        code.pseudop_jump_forward(the_end)

    code.pseudop_debug("before handlers")

    # TOS exception type, TOS-1 exception, TOS-2 backtrace
    for ca in normal_catches:
        ex, act = ca

        # each attempt to match the exception should have us at
        # the TOS,TOS-1,TOS-2 setup
        # PLUS the 3 from an exception block
        code.pseudop_faux_push(7)

        code.pseudop_position_of(ca)
        code.pseudop_label(label_next)

        label_next = code.gen_label()

        code.pseudop_debug("beginning of declared handler")

        if is_pair(ex):
            # The exception is intended to be bound to a local
            # variable. To achieve that, we're going to set up
            # a lambda with that single binding, and stuff out
            # handler code inside of it.

            if not is_proper(ex):
                raise code.error("non-proper biding in try", ex)

            match, (key, rest) = ex
            if rest:
                # leftover arguments
                raise code.error("too many bindings", ex)

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

            # handle the exception, return the result
            _helper_begin(code, act)
            code.pseudop_return()
            code.pseudop_faux_pop(4)  # trigger the finally

            # the return triggers the finally block to jump to
            # here. This ensures a value in the key and then
            # deletes it. Necessary just in case the handler
            # code also deleted the key (don't want to try
            # deleting it twice)
            code.pseudop_label(cleanup)
            code.pseudop_faux_push()  # the finally pushes a return
            code.pseudop_const(None)
            code.pseudop_set_var(key)
            code.pseudop_del_var(key)

            # end_finally pops our cleanup block and
            # finishes returning
            code.pseudop_end_finally()
            code.pseudop_return_none()

        else:
            # this is an exception match without a binding
            code.pseudop_dup()
            code.add_expression(ex)
            code.pseudop_compare_exception()
            code.pseudop_pop_jump_if_false(label_next)

            # Now let's throw all that stuff away.
            code.pseudop_pop()
            code.pseudop_pop()
            code.pseudop_pop()

            _helper_begin(code, act)
            code.pseudop_return()

        # yay we handled it!
        code.pseudop_pop_except()
        code.pseudop_debug("handled declared exception")
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
        # and return that value.
        code.pseudop_label(label_else)
        code.pseudop_debug("start of else handler")
        _helper_begin(code, act_else)

        # if there is a finally registered, this will trigger it
        # to run (and possibly overwrite the return value)
        code.pseudop_return()
        code.pseudop_debug("end of else handler")

    if has_finally:
        # here's the actual handling of the finally event. This
        # will get jumped to from the returns above, if the
        # finally block was registered.
        code.pseudop_label(the_end)
        code.pseudop_pop_block()

        code.pseudop_const(None)
        code.pseudop_faux_pop()

        # run the handler and overwrite the return value with its
        # result
        code.pseudop_label(label_finally)
        code.pseudop_faux_push(1)  # implicit from the finally block
        _helper_begin(code, act_finally)
        code.pseudop_return()

        # and close off the finally block
        code.pseudop_debug("closing off finally")
        code.pseudop_end_finally()

    else:
        code.pseudop_label(the_end)
        code.pseudop_return_none()

    code.pseudop_debug("at the end")

    # no further transformations needed on the passed source code.
    return None


@special(_symbol_setq)
def _special_setq(code, source):

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
def _special_global(code, source):

    called_by, (binding, rest) = source
    if not is_nil(rest):
        raise code.error("extra values in global lookup", source)

    code.pseudop_position_of(source)
    code.pseudop_get_global(str(binding))

    return None


@special(_symbol_define_global, _symbol_define)
def _special_define_global(code, source):

    called_by, (binding, body) = source

    _helper_begin(code, body)

    assert is_symbol(binding), "define-global with non-symbol binding"

    code.pseudop_position_of(source)
    code.pseudop_define_global(str(binding))

    # define expression evaluates to None
    code.pseudop_const(None)

    return None


@special(_symbol_define_local)
def _special_define_local(code, source):

    called_by, (binding, body) = source

    _helper_begin(code, body)

    assert is_symbol(binding), "define-local with non-symbol binding"

    code.pseudop_position_of(source)
    code.pseudop_define_local(str(binding))

    # define expression evaluates to None
    code.pseudop_const(None)

    return None


@special(_symbol_cond)
def _special_cond(code, source):

    called_by, cl = source

    done = code.gen_label()
    label = code.gen_label()

    for test, body in cl.unpack():
        code.pseudop_label(label)
        label = code.gen_label()

        if test is _symbol_else:
            # print(repr(body))
            _helper_begin(code, body)
            code.pseudop_jump_forward(done)
            break

        else:
            code.add_expression(test)
            code.pseudop_pop_jump_if_false(label)
            _helper_begin(code, body)
            code.pseudop_jump_forward(done)

    else:
        # there was no else statement, so add a catch-all
        code.pseudop_label(label)
        code.pseudop_const(None)

    code.pseudop_label(done)

    return None


# --- and finally clean up ---


__all__ = tuple(__all__)


#
# The end.
