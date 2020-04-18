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

Specials are sibilant forms which compile directly to bytecode, and
have no run-time form.

author: Christopher O'Brien <obriencj@gmail.com>
license: LGPL v.3
"""


from .lib import (
    symbol, is_symbol, keyword, is_keyword,
    is_lazygensym,
    nil, is_nil, cons, is_pair, is_proper,
    get_position, fill_position,
    trampoline, tailcall,
)

from .compiler import Special, gather_formals, gather_parameters, Mode

from textwrap import dedent


__all__ = []


_symbol__doc__ = symbol("__doc__")
_symbol_attr = symbol("attr")
_symbol_await = symbol("await")
_symbol_bang = symbol("!")
_symbol_begin = symbol("begin")
_symbol_build_proper = symbol("build-proper")
_symbol_bup = symbol("build-unpack-pair")
_symbol_break = symbol("break")
_symbol_cond = symbol("cond")
_symbol_cons = symbol("cons")
_symbol_continue = symbol("continue")
_symbol_define = symbol("define")
_symbol_define_global = symbol("define-global")
_symbol_define_values = symbol("define-values")
_symbol_del_attr = symbol("del-attr")
_symbol_delq = symbol("delq")
_symbol_delq_global = symbol("delq-global")
_symbol_doc = symbol("doc")
_symbol_for_each = symbol("for-each")
_symbol_function = symbol("function")
_symbol_global = symbol("global")
_symbol_import = symbol("import")
_symbol_import_from = symbol("import-from")
_symbol_keyword = symbol("keyword")
_symbol_lambda = symbol("lambda")
_symbol_let = symbol("let")
_symbol_method_call = symbol("method-call")
_symbol_nil = symbol("nil")
_symbol_quasiquote = symbol("quasiquote")
_symbol_quote = symbol("quote")
_symbol_refq = symbol("refq")
_symbol_return = symbol("return")
_symbol_set_attr = symbol("set-attr")
_symbol_setq = symbol("setq")
_symbol_setq_global = symbol("setq-global")
_symbol_setq_values = symbol("setq-values")
_symbol_splice = symbol("unquote-splicing")
_symbol_symbol = symbol("symbol")
_symbol_trampoline = symbol("trampoline")
_symbol_try = symbol("try")
_symbol_unquote = symbol("unquote")
_symbol_while = symbol("while")
_symbol_with = symbol("with")
_symbol_yield = symbol("yield")
_symbol_yield_from = symbol("yield-from")

_keyword_as = keyword("as")
_keyword_else = keyword("else")
_keyword_except = keyword("except")
_keyword_finally = keyword("finally")
_keyword_star = keyword("*")


def is_symbolish(thing):
    return is_symbol(thing) or is_lazygensym(thing)


def special(namesym, *aliases, glbls=globals()):
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

        return compilefn

    return deco


# --- special forms ---


def _helper_binding(code, source, required=True):

    msg = ("a binding must be in the form of (SYM EXPR) or (EXPR as: SYM),"
           " not %s" % source)

    if not is_proper(source):
        raise code.error(msg, source)

    sc = source.length()
    if sc == 3:
        expr, (_as, (sym, _rest)) = source
        if not (_as is _keyword_as and is_symbolish(sym)):
            raise code.error(msg, source)

    elif sc == 2:
        sym, (expr, _rest) = source
        if not is_symbolish(sym):
            raise code.error(msg, source)

    elif sc == 1 and not required:
        expr, _rest = source
        sym = required

    else:
        raise code.error(msg, source)

    return sym, expr


def _helper_keyword(code, kwd):
    """
    Pushes the pseudo ops necessary to put a keyword on the stack
    """

    code.pseudop_get_global(_symbol_keyword)
    code.pseudop_const(str(kwd))
    code.pseudop_call(1)
    return None


def _helper_symbol(code, sym):
    """
    Pushes the pseudo ops necessary to put a symbol on the stack
    """

    code.pseudop_get_global(_symbol_symbol)
    code.pseudop_const(str(sym))
    code.pseudop_call(1)
    return None


def _helper_nil(code):
    """
    Pushes the pseudo ops necessary to put a nil on the stack
    """

    # simple, but works. May want to do something other than a var
    # lookup in the future.

    code.pseudop_get_global(_symbol_nil)
    return None


@special(_symbol_doc)
def special_doc(code, source, tc=False):
    """
    (doc STR STR...)
    joins STR together and sets it as the docstr for the current scope
    """

    called_by, rest = source

    # collapse the doc
    docstr = "\n".join(d.strip() for d in map(str, rest.unpack()))
    docstr = dedent(docstr).strip()

    # set the module's docstring -- this works for module and class
    # definitions.
    code.declare_var(_symbol__doc__)
    code.pseudop_const(docstr)
    code.pseudop_set_var(_symbol__doc__)

    # force it as the code's docstring as well -- this works for
    # function definitions.
    code.set_doc(docstr)

    # doc special expression evaluates to None
    code.pseudop_const(None)

    return None


@special(_symbol_attr)
def special_get_attr(code, source, tc=False):
    """
    (attr OBJECT SYM)

    gets the attribute of the object that is named after the given
    symbol.
    """

    try:
        called_by, (obj, (member, rest)) = source
    except ValueError:
        raise code.error("too few arguments to attr", source)

    if not is_nil(rest):
        raise code.error("too many arguments to attr", source)

    if not is_symbolish(member):
        raise code.error("attr member must be a symbol", source)

    code.pseudop_position_of(source)
    code.add_expression(obj)
    code.pseudop_get_attr(member)

    # no further transformations
    return None


@special(_symbol_set_attr)
def special_set_attr(code, source, tc=False):
    """
    (set-attr OBJECT SYM EXPRESSION)

    sets the attribute of an object to the value of expression.

    this is also available via setf, eg.
    (setf (attr OBJ SYM) EXPRESSION)
    """

    try:
        called_by, (obj, (member, (value, rest))) = source
    except ValueError:
        raise code.error("too few arguments to set-attr", source)

    if not is_nil(rest):
        raise code.error("too many arguments to set-attr", source)

    code.add_expression(obj)
    code.add_expression(value)
    code.pseudop_rot_two()
    code.pseudop_set_attr(member)

    # make set-attr calls evaluate to None
    code.pseudop_const(None)

    # no further transformations
    return None


@special(_symbol_del_attr)
def special_del_attr(code, source, tc=False):
    """
    (del-attr OBJECT SYM)

    deletes the attribute from an object
    """

    try:
        called_by, (obj, (member, rest)) = source
    except ValueError:
        raise code.error("too few arguments to del-attr", source)

    if rest:
        raise code.error("too many arguments to del-attr", source)

    if not is_symbolish(member):
        raise code.error("del-attr member must be a symbol", source)

    code.pseudop_position_of(source)
    code.add_expression(obj)
    code.pseudop_del_attr(member)

    # make del-attr calls evaluate to None
    code.pseudop_const(None)

    # no further transformations
    return None


@special(_symbol_quote)
def special_quote(code, source, tc=False):
    """
    (quote FORM)

    Returns FORM without evaluating it.

    'FORM
    Same as (quote FORM)
    """

    called_by, body = source

    if not body:
        code.error("Too fuew arguments to quote %s" % source, source)

    body, rest = body

    if rest:
        code.error("Too many arguments to quote %s" % source, source)

    code.pseudop_position_of(source)
    _helper_quote(code, body)

    # no additional transform needed
    return None


def _helper_quote(code, body, tc=False):
    if body is nil:
        return _helper_nil(code)

    elif is_keyword(body):
        return _helper_keyword(code, body)

    elif is_symbolish(body):
        return _helper_symbol(code, body)

    elif is_pair(body):
        if is_proper(body):
            code.pseudop_get_var(_symbol_build_proper)
        else:
            code.pseudop_get_var(_symbol_cons)

        index = 0
        for index, expr in enumerate(body.unpack(), 1):
            _helper_quote(code, expr)

        code.pseudop_call(index)
        return None

    else:
        code.pseudop_const(body)
        return None


@special(_symbol_quasiquote)
def special_quasiquote(code, source, tc=False):
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


@trampoline
def _helper_quasiquote(code, marked, level=0):

    # print("_helper_quasiquote", repr(marked), "level=", level)

    if marked is nil:
        return tailcall(_helper_nil)(code)

    elif is_keyword(marked):
        return tailcall(_helper_keyword)(code, marked)

    elif is_symbolish(marked):
        return tailcall(_helper_symbol)(code, marked)

    elif is_pair(marked):
        if is_proper(marked):
            head, tail = marked

            # these handle cases where the nested unquote,
            # unquote-spicing, or quasiquote are the immediate
            # expression (as opposed to PART of the expression).

            if head is _symbol_unquote:
                tail, _rest = tail
                if level == 0:
                    return tailcall(code.add_expression)(tail)
                else:
                    code.pseudop_get_var(_symbol_build_proper)
                    _helper_symbol(code, head)
                    _helper_quasiquote(code, tail, level - 1)
                    code.pseudop_call(2)
                    return None

            elif head is _symbol_splice:
                tail, _rest = tail
                if level == 0:
                    code.pseudop_get_var(_symbol_bup)
                    code.add_expression(tail)
                    code.pseudop_call(1)
                    return None
                else:
                    code.pseudop_get_var(_symbol_build_proper)
                    _helper_symbol(code, head)
                    _helper_quasiquote(code, tail, level - 1)
                    code.pseudop_call(2)
                    return None

            elif head is _symbol_quasiquote:
                tail, _rest = tail
                code.pseudop_get_var(_symbol_build_proper)
                _helper_symbol(code, head)
                _helper_quasiquote(code, tail, level + 1)
                code.pseudop_call(2)
                return None

        # fall through for improper pairs, or pairs which aren't one
        # of the special invocations
        return tailcall(_helper_quasiquote_p)(code, marked, level)

    else:
        code.pseudop_const(marked)
        return None


def _helper_quasiquote_p(code, marked, level):
    coll_tup = 0  # number of completed tuples for build-unpack-pair
    curr_tup = 0  # items in the current WIP tuple

    def push_curr():
        # pushes a new item into current. Will also push the cons
        # call onto the stack first, if this is the first item
        nonlocal curr_tup
        # if not curr_tup:
        #    code.pseudop_get_var(symbol("cons"))
        curr_tup += 1

    def done_curr():
        # if there was a current, complete it and increment the
        # collected count.
        nonlocal coll_tup
        nonlocal curr_tup

        if not curr_tup:
            return

        # code.pseudop_call(curr_tup)
        code.pseudop_build_tuple(curr_tup)
        coll_tup += 1
        curr_tup = 0

    def push_coll():
        # if there was a current, complete it and increment the
        # collected count. Then increment the collected count again.
        nonlocal coll_tup
        done_curr()
        coll_tup += 1

    code.pseudop_get_var(_symbol_bup)

    for p_expr in marked.follow():
        if p_expr is nil:
            push_coll()
            _helper_nil(code)
            break

        elif not is_pair(p_expr):
            push_curr()
            _helper_quasiquote(code, p_expr, level)
            continue

        expr, p_tail = p_expr

        if expr is nil:
            push_curr()
            _helper_nil(code)
            continue

        elif is_symbolish(expr):
            if expr is _symbol_unquote:
                u_expr, tail = p_tail

                if level == 0:
                    push_curr()
                    code.add_expression(u_expr)
                    break
                else:
                    push_curr()
                    code.pseudop_get_var(_symbol_build_proper)
                    _helper_symbol(code, expr)
                    _helper_quasiquote(code, u_expr, level - 1)
                    code.pseudop_call(2)
                    break

            elif expr is _symbol_quasiquote:
                u_expr, tail = p_expr

                push_curr()
                code.pseudop_get_var(_symbol_build_proper)
                _helper_symbol(code, expr)
                _helper_quasiquote(code, u_expr, level + 1)
                code.pseudop_call(2)
                break

            elif expr is _symbol_splice:
                u_expr, tail = p_tail

                if level == 0:
                    push_coll()
                    code.add_expression(u_expr)
                    break
                else:
                    push_curr()
                    code.pseudop_get_var(_symbol_build_proper)
                    _helper_symbol(code, expr)
                    _helper_quasiquote(code, u_expr, level - 1)
                    code.pseudop_call(2)
                    break

            else:
                push_curr()
                _helper_symbol(code, expr)
                continue

        elif is_keyword(expr):
            push_curr()
            _helper_keyword(code, expr)
            continue

        elif is_pair(expr):
            if is_proper(expr):
                head, tail = expr

                if head is _symbol_unquote:
                    u_expr, tail = tail

                    if level == 0:
                        push_curr()
                        code.add_expression(u_expr)
                        continue
                    else:
                        push_curr()
                        code.pseudop_get_var(_symbol_build_proper)
                        _helper_symbol(code, head)
                        _helper_quasiquote(code, u_expr, level - 1)
                        code.pseudop_call(2)
                        continue

                elif head is _symbol_quasiquote:
                    u_expr, tail = tail

                    push_curr()
                    code.pseudop_get_var(_symbol_build_proper)
                    _helper_symbol(code, head)
                    _helper_quasiquote(code, u_expr, level + 1)
                    code.pseudop_call(2)
                    continue

                elif head is _symbol_splice:
                    u_expr, tail = tail

                    # todo: if tail, err, since splice only takes one arg

                    if level == 0:
                        push_coll()
                        code.add_expression(u_expr)
                        continue
                    else:
                        push_curr()
                        code.pseudop_get_var(_symbol_build_proper)
                        _helper_symbol(code, head)
                        _helper_quasiquote(code, u_expr, level - 1)
                        code.pseudop_call(2)
                        continue

            push_curr()
            _helper_quasiquote_p(code, expr, level)

        else:
            push_curr()
            _helper_quasiquote(code, expr, level)

    done_curr()

    # invoke the build-unpack-pair function with the number of
    # collected tuples we've pushed onto the stack.
    code.pseudop_call(coll_tup)
    return None


@special(_symbol_begin)
def special_begin(code, source, tc=False):
    """
    (begin EXPR EXPR...)

    Evaluates each EXPR in turn. Returns the last result only.

    (begin)

    Evaluates to None
    """

    called_by, body = source

    _helper_begin(code, body, tc)

    # no additional transform needed
    return None


def _helper_begin(code, body, tc):
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
def special_with(code, source, tc=False):
    """
    (with (BINDING EXPRESSION) BODY...)

    Enters the context manager from EXPRESSION, binding it to BINDING,
    and evaluates the expressions of BODY. The final value of BODY is
    the result.

    Alternative binding syntax is supported, eg.
    (with (EXPRESSION as: BINDING) BODY...)
    """

    called_by, (args, body) = source

    binding, expr = _helper_binding(code, args)

    # binding = str(binding)
    code.declare_var(binding)

    storage = code.gensym("with")
    code.declare_var(storage)

    with code.block_with(expr):
        code.pseudop_set_var(binding)
        _helper_begin(code, body, False)
        code.pseudop_set_var(storage)

    code.pseudop_debug("before fetching storage in special_with")
    code.pseudop_get_var(storage)
    code.pseudop_del_var(storage)

    return None


@special(_symbol_lambda)
def special_lambda(code, source, tc=False):
    """
    (lambda (FORMAL...) BODY...)

    Creates an anonymous function taking FORMAL arguments, and executing
    the BODY expressions in order. The result of the final expression is
    returned.
    """

    called_by, (args, body) = source

    code.pseudop_position_of(source)

    _helper_function(code, "<lambda>", args, body,
                     declared_at=source.get_position())

    # no additional transform needed
    return None


@special(_symbol_function)
def special_function(code, source, tc=False):
    """
    (function NAME (FORMAL...) BODY...)

    Creates a function with a binding to itself as NAME, taking FORMAL
    arguments, and executing the BODY expressions in order. The result
    of the final expression is returned.
    """

    called_by, (namesym, cl) = source
    args, body = cl

    # create the function inside of a closure that has two local
    # cells, which is the new function's name and an empty-string
    # name. this will give the function the ability to reference
    # itself via that cell, and also gives us a way to get a reference
    # to the function itself for tailcall recusion testing.

    name = str(namesym)
    declared_at = source.get_position()

    code.pseudop_position_of(source)

    kid = code.child_context(declared_at=declared_at)
    with kid as subc:
        self_ref = subc.gensym(namesym)
        subc.declare_var(self_ref)
        subc.declare_var(namesym)

        _helper_function(subc, name, args, body,
                         self_ref=self_ref,
                         declared_at=declared_at)

        subc.pseudop_dup()
        subc.pseudop_set_var(self_ref)
        subc.pseudop_dup()
        subc.pseudop_set_var(namesym)

        subc.pseudop_return()
        kid_code = subc.complete()

    # kid_code returns our function object
    code.pseudop_lambda(kid_code)
    code.pseudop_call(0)

    # no additional transform needed
    return None


@special(_symbol_let)
def special_let(code, source, tc=False):
    """
    (let ((BINDING EXPR) ...) BODY...)

    Creates a lexical scope where each BINDING is assigned the value from
    evaluating EXPR, then executes the BODY expressions in order. The
    result of the final expression is returned.

    (let NAME ((BINDING EXPR) ...) BODY...)

    Same as above, but also binds a recursive function NAME which
    re-enters the LET with updated binding values.

    In both cases, the alternative binding syntax may be used, eg.
    (let ((EXPR as: BINDING) ...) BODY...)
    """

    called_by, rest = source

    bindings, body = rest
    if is_symbolish(bindings):
        named = bindings
        bindings, body = body
    else:
        named = None

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

    declared_at = source.get_position()
    if declared_at:
        code.pseudop_position(*declared_at)

    if named:
        # wrap a really short lambda around the let, in order to give
        # it a binding to itself by its name as a freevar
        kid = code.child_context(declared_at=declared_at)
        with kid as subc:
            self_ref = subc.gensym(named)
            subc.declare_var(self_ref)
            subc.declare_var(named)

            fnamed = "<let %s>" % named
            _helper_function(subc, fnamed, args, body,
                             self_ref=self_ref,
                             declared_at=declared_at)

            subc.pseudop_dup()
            subc.pseudop_set_var(self_ref)
            subc.pseudop_dup()
            subc.pseudop_set_var(named)
            subc.pseudop_return()
            kid_code = subc.complete()

        code.pseudop_lambda(kid_code)
        code.pseudop_call(0)

    else:
        _helper_function(code, "<let>", args, body,
                         declared_at=declared_at)

    # after both the named or unnamed variations, we now have the let
    # bound as a callable at TOS

    pvals = cons(*vals, nil) if vals else nil
    code.complete_apply(pvals, declared_at, tc, lambda e, t: None)

    # no additional transform needed
    return None


def _helper_function(code, name, args, body,
                     self_ref=None, declared_at=None):

    tco = code.tco_enabled

    if not isinstance(name, (symbol, str)):
        msg = "function names must be symbol or str, not %r" % name
        raise code.error(msg, declared_at)

    formals = gather_formals(args, get_position(args, declared_at))
    pos, defaults, kwonly, star, starstar = formals

    # argnames = list(map(str, pos))
    argnames = list(pos)

    if defaults:
        argnames.extend(symbol(k[0]) for k in defaults)

    if kwonly:
        argnames.extend(symbol(k[0]) for k in kwonly)

    if star:
        varargs = True
        argnames.append(star)
    else:
        varargs = False

    if starstar:
        varkeywords = True
        argnames.append(starstar)
    else:
        varkeywords = False

    if declared_at is None:
        declared_at = body.get_position()

    kid = code.child_context(name=name,
                             self_ref=self_ref,
                             args=argnames,
                             kwonly=len(kwonly),
                             varargs=varargs,
                             varkeywords=varkeywords,
                             declared_at=declared_at,
                             tco_enabled=tco)

    with kid as subc:
        body, doc = _helper_strip_doc(body)
        subc.set_doc(doc)

        _helper_begin(subc, body, tco)
        subc.pseudop_return()
        code.pseudop_lambda(subc.complete(), defaults, kwonly)

        tco = subc.tco_enabled and not subc.generator and subc.tailcalls
        if tco:
            code.pseudop_get_var(_symbol_trampoline)
            code.pseudop_rot_two()
            code.pseudop_call(1)


def _helper_strip_doc(body):
    """
    This helper function will identify any string literals at the
    beginning of a collection of source expressions, and separate it
    out.
    """

    doc = None

    if body:
        front, rest = body
        if rest and isinstance(front, str):
            doc = dedent(front).strip()
            body = rest

    return body, doc


@special(_symbol_while)
def special_while(code, source, tc=False):
    """
    (while TEST_EXPR BODY...)

    Evaluates TEST_EXPR. If the result is truthful, evaluates the BODY
    expressions in order, then repeats until the evaluation of
    TEST_EXPR is not truthful. The result is specified via the break
    expression, the most recent continue expression, or the final
    expression of the BODY

    (while TEST_EXPR
       ...
       (continue VAL))

    The continue expression pops off all extra values on the stack and
    stores VAL as a possible result for the loop. If the loop
    terminates naturally (by TEST_EXPR failing), this will be the
    result.

    (while TEST_EXPR
       ...
       (break VAL))

    The break expression terminates the loop with the given value. If
    VAL is not specified, it defaults to None
    """

    try:
        called_by, (test, body) = source
    except ValueError:
        raise code.error("too few arguments to while", source)

    storage = code.gensym("while")
    code.declare_var(storage)

    # initial value, just in case we never actually loop
    code.pseudop_const(None)
    code.pseudop_set_var(storage)

    with code.block_loop() as block:
        # this enables continue and break to find it and set it
        block.storage = storage

        code.add_expression(test)
        code.pseudop_pop_jump_if_false(block.pop_label)

        whatever = code.gen_label()
        with code.block_finally(whatever):
            _helper_begin(code, body, False)
            code.pseudop_set_var(storage)

        with code.block_finally_cleanup(whatever):
            # this is required by the finally above. The finally is
            # only present so that we can get the stack to unwind if
            # continue is called. break would already have unwound the
            # stack, but continue doesn't for whatever reason.
            pass

        code.pseudop_jump(block.top_label)

    code.pseudop_get_var(storage)
    code.pseudop_const(None)
    code.pseudop_set_var(storage)
    code.pseudop_del_var(storage)

    return None


@special(_symbol_for_each)
def special_for_each(code, source, tc=False):
    """
    (for-each (BINDING  ITEREXPR) . BODY)
    """

    try:
        called_by, (bindings, body) = source
        bindings, (expr, rest) = bindings
    except ValueError:
        raise code.error("too few arguments to for-each", source)

    if not is_nil(rest):
        raise code.error("too many arguments to for-each", source)

    storage = code.gensym("for-each")
    code.declare_var(storage)

    code.pseudop_const(None)
    code.pseudop_set_var(storage)

    next_label = code.gen_label()

    with code.block_loop() as block:
        # this enables continue and break to find it and set it
        block.storage = storage

        code.add_expression(expr, False)
        code.pseudop_iter()

        code.pseudop_label(next_label)
        code.pseudop_for_iter(block.pop_label)

        _helper_setq_values(code, bindings, True)

        whatever = code.gen_label()
        with code.block_finally(whatever):
            _helper_begin(code, body, False)
            code.pseudop_set_var(storage)

        with code.block_finally_cleanup(whatever):
            pass

        code.pseudop_jump(next_label)
        code.pseudop_faux_pop()

    code.pseudop_get_var(storage)
    code.pseudop_const(None)
    code.pseudop_set_var(storage)
    code.pseudop_del_var(storage)

    return None


@special(_symbol_setq_values)
def special_setq_values(code, source, tc=False):
    """
    (setq-values BINDINGS VALUES_EXPR)

    where BINDINGS is a nested pair of symbols in the same structure
    as values from VALUES_EXPR.
    """

    try:
        called_by, (bindings, (expr, rest)) = source
    except ValueError:
        raise code.error("too few arguments to setq-values", source)

    if not is_nil(rest):
        raise code.error("too many arguments to setq-values", source)

    code.add_expression(expr, False)
    _helper_setq_values(code, bindings, False)

    # setq-values evaluates to None
    code.pseudop_const(None)

    return None


@special(_symbol_define_values)
def special_define_values(code, source, tc=False):
    """
    (define-values BINDINGS VALUES_EXPR)

    where BINDINGS is a nested pair of symbols in the same structure
    as values from VALS. Each binding will be declared in the local
    scope prior to assignment.
    """

    try:
        called_by, (bindings, (expr, rest)) = source
    except ValueError:
        raise code.error("too few arguments to define-values", source)

    if not is_nil(rest):
        raise code.error("too many arguments to define-values", source)

    code.add_expression(expr, False)
    _helper_setq_values(code, bindings, True)

    # define-values evaluates to None
    code.pseudop_const(None)

    return None


def _helper_values_binding(code, bindings):
    left = []
    mid = None
    right = []

    biter = iter(bindings.unpack())

    for b in biter:
        if is_symbolish(b) or is_pair(b):
            left.append(b)

        elif b is _keyword_star:
            mid = next(biter, None)
            if mid is None:
                msg = "expected symbol binding after *: not %r" % mid
                raise code.error(msg, bindings)
            break

        else:
            msg = "expected symbol or pair in binding, not %r" % b
            raise code.error(msg, bindings)

    for b in biter:
        if is_symbolish(b) or is_pair(b):
            right.append(b)

        else:
            msg = "expected symbol or pair in binding, not %r" % b
            raise code.error(msg, bindings)

    return left, mid, right


def _helper_setq_values(code, bindings, declare):
    if is_symbolish(bindings):
        if declare:
            code.declare_var(bindings)
        code.pseudop_set_var(bindings)
        return

    bcount = bindings.length()

    if is_nil(bindings):
        code.pseudop_pop()

    elif is_proper(bindings):
        left, mid, right = _helper_values_binding(code, bindings)
        if mid:
            code.pseudop_unpack_ex(len(left), len(right))

            for b in left:
                if is_symbolish(b):
                    if declare:
                        code.declare_var(b)
                    code.pseudop_set_var(b)
                elif is_pair(b):
                    _helper_setq_values(code, b, declare)
                else:
                    msg = "bindings must be symbols or pairs, not %r" % b
                    raise code.error(msg, bindings)

            if is_symbolish(mid):
                if declare:
                    code.declare_var(mid)
                code.pseudop_set_var(mid)
            else:
                msg = "start bindings must be symbols, not %r" % mid
                raise code.error(msg, bindings)

            for b in right:
                if is_symbolish(b):
                    if declare:
                        code.declare_var(b)
                    code.pseudop_set_var(b)
                elif is_pair(b):
                    _helper_setq_values(code, b, declare)
                else:
                    msg = "bindings must be symbols or pairs, not %r" % b
                    raise code.error(msg, bindings)

        else:
            code.pseudop_unpack_sequence(bcount)
            for b in bindings.unpack():
                if is_symbolish(b):
                    if declare:
                        code.declare_var(b)
                    code.pseudop_set_var(b)
                elif is_pair(b):
                    _helper_setq_values(code, b, declare)
                else:
                    msg = "bindings must be symbols or pairs, not %r" % b
                    raise code.error(msg, bindings)

    else:
        code.pseudop_unpack_ex(bcount - 1, 0)
        for b in bindings.unpack():
            if is_symbolish(b):
                if declare:
                    code.declare_var(b)
                code.pseudop_set_var(b)
            elif is_pair(b):
                _helper_setq_values(code, b, declare)
            else:
                msg = "improper bindings must be symbols or pairs, not %r" % b
                raise code.error(msg, bindings)


@special(_symbol_continue)
def special_continue(code, source, tc=False):

    from sibilant.pseudops import Block

    called_by, rest = source

    for block in reversed(code.blocks):
        if block.block_type is Block.LOOP:
            break
    else:
        raise code.error("continue called without while", source)


    if is_nil(rest):
        value = None

    else:
        value, _rest = rest
        if not is_nil(_rest):
            msg = "Too many arguments to %s, %r" % (called_by, rest)
            raise code.error(msg, source)
        rest = _rest

    # this magic causes a number of pops to be inserted equal to the
    # current stack counter. It's calculated when max_stack is run.
    # code.pseudop_magic_pop_all()

    code.add_expression(value, False)
    code.pseudop_set_var(block.storage)
    code.pseudop_continue_loop(block.top_label)

    return None


@special(_symbol_break)
def special_break(code, source, tc=False):

    from sibilant.pseudops import Block

    called_by, rest = source

    for block in reversed(code.blocks):
        if block.block_type is Block.LOOP:
            break
    else:
        raise code.error("break called without while", source)

    if is_nil(rest):
        value = None

    else:
        value, _rest = rest
        if not is_nil(_rest):
            msg = "Too many arguments to %s, %r" % (called_by, rest)
            raise code.error(msg, source)
        rest = _rest

    code.add_expression(value, False)
    code.pseudop_set_var(block.storage)
    code.pseudop_break_loop()

    return None


@special(_symbol_return)
def special_return(code, source, tc=False):

    called_by, rest = source

    if is_nil(rest):
        value = None
        code.pseudop_return_none()

    else:
        value, _rest = rest
        if not is_nil(_rest):
            msg = "Too many arguments to %s, %r" % (called_by, rest)
            raise code.error(msg, source)
        rest = _rest

        # the expression being returned is always valid tco if the
        # code is thusly enabled
        code.add_expression(value, tc)
        code.pseudop_return()

    # just to calm max-stack down, we'll pretend return is a perfectly
    # valid expression which evaluates to a result.
    code.pseudop_faux_push()

    return None


@special(_symbol_yield)
def special_yield(code, source, tc=False):

    called_by, rest = source

    if is_nil(rest):
        value = None
        code.pseudop_yield_none()

    else:
        value, _rest = rest
        if not is_nil(_rest):
            msg = "Too many arguments to %s, %r" % (called_by, rest)
            raise code.error(msg, source)
        rest = _rest

        # the expression being yielded is always NOT tco valid
        code.add_expression(value, False)
        code.pseudop_yield()

    # yield op does indeed push a value back onto the stack, and
    # max-stack knows this. No need to try and fake a result.

    return None


def _helper_yield_from(code, source):

    try:
        called_by, (value, rest) = source

    except ValueError:
        msg = "Too few arguments to %s, %r" % (called_by, source)
        raise code.error(msg, source)

    # the expression being yielded from is always NOT tco valid
    code.add_expression(value, False)

    # according to the dis page, I should only need to call
    # YIELD_FROM, but that always fails. If I look at a disassembly of
    # a simple yield from call in Python, I see that there's a const
    # None at TOS, with the generator below it whenever YIELD_FROM
    # happens. I'm emulating that here, with the additional option to
    # send the result of any expression to the generator/coroutine,
    # rather than just None.

    if not is_nil(rest):

        try:
            kw, arg = rest.unpack()

        except ValueError:
            # helpful comment describing helpful error message
            msg = "Wrong arguments to %s, %r" % (called_by, source)
            raise code.error(msg, source)

        if kw == keyword("send"):
            return arg

        else:
            msg = "Too many arguments to %s, %r" % (called_by, source)
            raise code.error(msg, source)

    else:
        return None


@special(_symbol_await)
def special_await(code, source, tc=False):
    send_value = _helper_yield_from(code, source)

    code.pseudop_get_awaitable()
    # see comment in _helper_yield_from()
    code.add_expression(send_value)
    code.pseudop_yield_from()

    return None


@special(_symbol_yield_from)
def special_yield_from(code, source, tc=False):
    send_value = _helper_yield_from(code, source)

    code.pseudop_get_yield_from_iter()
    # see comment in _helper_yield_from()
    code.add_expression(send_value)
    code.pseudop_yield_from()
    code.pseudop_pop()

    # unlike yield, yield-from doesn't have a real result value, so
    # we'll give it one.
    code.pseudop_const(None)

    return None


@special(_symbol_try)
def special_try(code, source, tc=False):
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
    not return. finally does not override the result value -- whether
    from a completed EXPR or a matching EXC_HANDLER.
    """

    called_by, (try_expr, catches) = source

    ca, act_else, act_finally = _collect_catches(code, catches)

    storage = code.gensym("try")
    code.declare_var(storage)

    if act_finally:
        if act_else:
            _try_else_finally(code, try_expr, ca, storage,
                              act_else, act_finally, tc)
        else:
            _try_finally(code, try_expr, ca, storage,
                         act_finally, tc)

    elif act_else:
        _try_else(code, try_expr, ca, storage,
                  act_else, tc)

    else:
        _try(code, try_expr, ca, storage, tc)

    code.pseudop_get_var(storage)
    code.pseudop_const(None)
    code.pseudop_set_var(storage)
    code.pseudop_del_var(storage)


def _collect_catches(code, catches):
    # returns (normal_catches, act_else, act_finally)
    #
    # - normal_catches is a series of (cons (values SYMBOL EXPR), act, nil)
    # - act_else is None or a cons that is the body of the else clause
    # - act_finally is None or a cons that is the body of the finally clause

    normal_catches = []
    act_else = None
    act_finally = None

    # first, filter our catches down into normal exception
    # matches, finally, and else
    for ca in catches.unpack():
        if not ca:
            raise code.error("clause with no body in try", ca)

        ex, act = ca
        if is_proper(ex):
            # [[err Exception] body…]
            # [[Exception as: err] body…]
            ex_binding, ex_expr = _helper_binding(code, ex, None)

            ex = cons([ex_binding, ex_expr], act)
            fill_position(ex, ca.get_position())
            normal_catches.append(ex)

        elif ex is _keyword_except:
            # [except: Exception body…]
            try:
                ex_type, act = act
            except ValueError:
                raise code.error("malformed except: clause in try", ca)

            ex = cons([ex, ex_type], act)
            fill_position(ex, ca.get_position())
            normal_catches.append(ex)

        elif ex is _keyword_finally:
            # [finally: body…]
            if act_finally:
                raise code.error("duplicate finally: clause in try", ca)

            act_finally = act

        elif ex is _keyword_else:
            # [else: body…]
            if act_else:
                raise code.error("duplicate else: clause in try", ca)

            act_else = act

        else:
            raise code.error("missing keyword label in else/finally", ca)

    return normal_catches, act_else, act_finally


def _try(code, try_expr, catches, storage, tc):
    except_label = code.gen_label()
    end_label = code.gen_label()

    with code.block_try(except_label):
        code.add_expression(try_expr, False)
        code.pseudop_set_var(storage)

    code.pseudop_jump_forward(end_label)

    _except(code, catches, except_label, end_label, storage, tc)

    code.pseudop_label(end_label)


def _try_else(code, try_expr, catches, storage,
              else_exprs, tc):

    except_label = code.gen_label()
    else_label = code.gen_label()
    end_label = code.gen_label()

    with code.block_try(except_label):
        code.add_expression(try_expr, False)
        code.pseudop_pop()

    # it would be nice to just handle the else here and then jump to
    # the end but that would mean the bytecode line numbers wouldn't
    # be linear anymore, and python3.5 doesn't support that sort of
    # thing. So we have to jump past the catch code to the else block.
    code.pseudop_jump_forward(else_label)

    _except(code, catches, except_label, end_label, storage, tc)

    code.pseudop_label(else_label)
    _helper_begin(code, else_exprs, tc)
    code.pseudop_set_var(storage)

    code.pseudop_label(end_label)


def _try_finally(code, try_expr, catches, storage,
                 fin_exprs, tc):

    cleanup = code.gen_label()
    with code.block_finally(cleanup):
        _try(code, try_expr, catches, storage, False)

    with code.block_finally_cleanup(cleanup):
        _helper_begin(code, fin_exprs, tc)
        code.pseudop_pop()


def _try_else_finally(code, try_expr, catches, storage,
                      else_exprs, fin_exprs, tc):

    cleanup = code.gen_label()
    with code.block_finally(cleanup):
        _try_else(code, try_expr, catches, storage, else_exprs, False)

    with code.block_finally_cleanup(cleanup):
        _helper_begin(code, fin_exprs, tc)
        code.pseudop_pop()


def _except(code, catches, except_label, end_label,
            storage, tc):

    with code.block_except(except_label):
        curr_label = except_label
        next_label = code.gen_label()

        for ca in catches:
            (key, match), act = ca
            # this makes the following two formats equivalent:
            # [[Exception] body…]
            # [except: Exception body…]
            if key == _keyword_except:
                key = None

            code.pseudop_position_of(ca)
            _except_match(code, key, match,
                          curr_label, next_label, end_label,
                          act, storage, tc)

            curr_label = next_label
            next_label = code.gen_label()

        # after the loop, drop a label for the fall-through.
        code.pseudop_label(curr_label)

    # block closes, anything that reached the fall-through is re-raised
    pass


def _except_match(code, key, match,
                  match_label, next_label, end_label,
                  act, storage, tc=False):

    with code.block_except_match(match_label):
        code.pseudop_dup()
        code.add_expression(match, False)
        code.pseudop_compare_exception()
        code.pseudop_pop_jump_if_false(next_label)

        if key:
            # key = str(key)
            code.declare_var(key)

            code.pseudop_pop()
            code.pseudop_set_var(key)
            code.pseudop_pop()
            code.pseudop_debug("exc with key, should be zero")

            cleanup = code.gen_label()
            with code.block_finally(cleanup):
                _helper_begin(code, act, tc)
                code.pseudop_set_var(storage)

            with code.block_finally_cleanup(cleanup):
                code.pseudop_const(None)
                code.pseudop_set_var(key)
                code.pseudop_del_var(key)

        else:
            code.pseudop_pop(3)
            code.pseudop_debug("exc, no key, should be zero")

            _helper_begin(code, act, tc)
            code.pseudop_set_var(storage)

        code.pseudop_debug("just propr to pop_except")

    # post pop-except
    code.pseudop_jump_forward(end_label)


@special(_symbol_setq)
def special_setq(code, source, tc=False):
    """
    (setq SYM EXPR)

    binds the result of evaluating EXPR to SYM
    """

    called_by, (binding, body) = source

    if not is_symbolish(binding):
        raise code.error("assignment must be by symbolic name",
                         source)

    value, rest = body
    if rest:
        raise code.error("extra values in assignment", source)

    if not is_pair(value):
        code.pseudop_position_of(body)

    code.add_expression(value)
    code.pseudop_set_var(binding)

    # set-var calls should evaluate to None
    code.pseudop_const(None)

    # no additional transform needed
    return None


@special(_symbol_delq)
def special_delq(code, source, tc=False):
    """
    (delq SYM)

    unbinds the given SYM
    """

    called_by, (binding, rest) = source

    if not is_symbolish(binding):
        raise code.error("delq must be by symbolic name", source)

    if rest:
        raise code.error("extra arguments to delq", source)

    code.pseudop_del_var(binding)

    # del-var calls should evaluate to None
    code.pseudop_const(None)

    # no additional transform needed
    return None


@special(_symbol_refq)
def special_refq(code, source, tc=False):
    """
    (refq SYM)

    forces a closure cell for the given SYM, returns the cell.
    """

    try:
        called_by, (binding, rest) = source
    except ValueError:
        raise code.error("not enough arguments to refq", source)
    if rest:
        raise code.error("too many arguments to refq", source)
    if not is_symbolish(binding):
        raise code.error("refq must be by symbolic name", source)

    if not code.request_cell(binding):
        raise code.error("refq could not find a binding", source)

    code.pseudop_load_cell(binding)

    return None


@special(_symbol_global)
def special_global(code, source, tc=False):
    """
    (global SYM)
    Evaluates a symbol in the global scope. This is useful if the
    symbol is shadowed by a local binding.
    """

    try:
        called_by, (binding, rest) = source
    except ValueError:
        raise code.error("missing symbol for global lookup", source)

    if rest:
        raise code.error("extra values in global lookup", source)

    code.pseudop_position_of(source)
    code.pseudop_get_global(binding)

    return None


@special(_symbol_delq_global)
def special_delq_global(code, source, tc=False):
    """
    (delq-global SYM)

    Removes a binding from the global context.
    """

    try:
        called_by, (binding, rest) = source
    except ValueError:
        raise code.error("missing symbol for delq-global", source)

    if rest:
        raise code.error("extra values in delq-global", source)

    code.pseudop_position_of(source)
    code.pseudop_del_global(binding)

    code.pseudop_const(None)

    return None


@special(_symbol_define_global, _symbol_setq_global)
def special_define_global(code, source, tc=False):
    """
    (define-global SYM EXPRESSION)
    (setq-global SYM EXPRESSION)

    Defines or sets a value in global context. If EXPRESSION is
    omitted, it defaults to None.
    """

    try:
        called_by, (binding, body) = source
    except ValueError:
        raise code.error("too few arguments to define-global", source)

    if not is_symbolish(binding):
        raise code.error("define-global with non-symbol binding", source)

    if body:
        body, rest = body
        if rest:
            raise code.error("too many arguments to define-global", source)

        code.add_expression(body, False)
    else:
        code.pseudop_const(None)

    code.pseudop_position_of(source)
    code.pseudop_set_global(binding)

    # define expression evaluates to None
    code.pseudop_const(None)

    return None


@special(_symbol_define)
def special_define(code, source, tc=False):
    """
    (define SYM)
    (define SYM EXPRESSION)

    Defines or sets a value in a local context. If EXPRESSION is
    omitted, the symbol is declared but unassigned.
    """

    if code.mode is Mode.MODULE:
        return special_define_global(code, source, tc)

    try:
        called_by, (binding, body) = source
    except ValueError:
        raise code.error("too few arguments to define", source)

    if not is_symbolish(binding):
        raise code.error("define with non-symbol binding", source)

    code.declare_var(binding)

    if body:
        body, rest = body
        if rest is not nil:
            raise code.error("too many arguments to define", source)

        code.add_expression(body, False)
        code.pseudop_position_of(source)
        code.pseudop_set_var(binding)

    # define expression evaluates to None
    code.pseudop_const(None)

    return None


@special(_symbol_cond)
def special_cond(code, source, tc=False):
    """
    (cond (TEST_EXPRESSION BODY...)... )
    Conditionally executes body depending on a test expression. Multiple
    test expressions and bodies may be specified, but only the body of the
    first truthful expression is evaluated and returned. If no test
    expression is truthful, evaluates to None.

    (cond (TEST_EXPRESSION BODY...)
          ...
          (else: BODY...))

    As above, but the else keyword is considered true, and the
    associated body will be evaluated and returned if no other
    previous test expression matched. Identical behavior can be
    achieved by using a truthful constant (eg. True, or 1) as the test
    expression.
    """

    called_by, cl = source

    done = code.gen_label()
    label = code.gen_label()

    for test, body in cl.unpack():
        code.pseudop_label(label)
        label = code.gen_label()

        if test is _keyword_else:
            # with code.block_begin():
            _helper_begin(code, body, tc)
            # code.pseudop_jump_forward(done)
            break

        else:
            code.add_expression(test)
            code.pseudop_pop_jump_if_false(label)
            # with code.block_begin():
            _helper_begin(code, body, tc)
            code.pseudop_jump_forward(done)

    else:
        # there was no else statement, so add a catch-all
        code.pseudop_label(label)
        code.pseudop_const(None)

    code.pseudop_label(done)

    return None


def _helper_import_level(wanted: symbol):
    """
    Convert a symbol with preceeding dots into a tuple of a count of
    dots and a symbol sans preceeding dots.

    eg.  symbol(".foo")  ->  (1, symbol("foo"))
    """

    o_wanted = str(wanted)

    r_wanted = o_wanted.lstrip(".")
    w_level = len(o_wanted) - len(r_wanted)

    return w_level, symbol(r_wanted)


@special(_symbol_import)
def special_import(code, source, tc=False):
    """
    (import NAME)

    Evaluates to a reference to module named NAME. If NAME is a dotted
    import, performs the import and evanualtes to the topmost module.
    """

    try:
        called_by, (name, rest) = source
    except ValueError:
        raise code.error("too few arguments to import", source)

    if rest:
        raise code.error("too many arguments to import", source)

    if not isinstance(name, (symbol, str)):
        raise code.error("import argument must be symbol", source)

    level, name = _helper_import_level(name)

    code.pseudop_const(level)
    code.pseudop_const(None)
    code.pseudop_import_name(name)

    return None


@special(_symbol_import_from)
def special_import_from(code, source, tc=False):
    """
    (import-from NAME MEMBER...)

    imports NAME and returns a tuple of matching MEMBER attributes
    """

    try:
        called_by, (name, rest) = source
    except ValueError:
        raise code.error("too few arguments to import-from", source)

    if rest is nil:
        raise code.error("too few arguments to import-from", source)

    if not isinstance(name, (symbol, str)):
        raise code.error("import argument must be symbol", source)

    level, name = _helper_import_level(name)

    # the LEVEL value
    code.pseudop_const(level)

    members = list()
    for mp in rest.follow():
        if mp is nil:
            break

        member, _tail = mp
        if not is_symbolish(member):
            msg = "import-from members must be symbols, not %r" % member
            raise code.error(msg, mp)

        # member = str(member)
        members.append(member)
        code.pseudop_const(str(member))

    # the FROMLIST tuple
    code.pseudop_build_tuple(len(members))

    # do the actual import to get the module onto TOS
    code.pseudop_import_name(name)

    for member in members:
        code.pseudop_import_from(member)
        code.pseudop_rot_two()

    # import_from leaves the module on the stack. When we're done with
    # it, get rid of it!
    code.pseudop_pop()

    code.pseudop_build_tuple(len(members))

    return None


@special(_symbol_method_call, _symbol_bang)
def special_method_call(code, source, tc=False):
    """
    (! METHODSYM OBJEXPR ARGS...)
    """

    # TODO: this doesn't currently tailcall if it's in the tail
    # position, it only calls in-line. Should this turn into a
    # tailcall_full in the event that it's in the tail position?

    try:
        called_by, (name, (obj, args)) = source
    except ValueError:
        raise code.error("too few arguments to method-call", source)

    if not is_symbolish(name):
        msg = "method name must be symbol, not %r" % name
        raise code.error(msg, source)

    pos, kwds, kvals, star, starstar = gather_parameters(args)

    if kwds or star or starstar:
        # method_call can't handle keyword args, only positionals
        return cons(cons(_symbol_attr, obj, name, nil), args)

    code.add_expression(obj)
    code.pseudop_get_method(name)

    for arg in pos:
        code.add_expression(arg)
    code.pseudop_call_method(len(pos))

    return None


#
# The end.
