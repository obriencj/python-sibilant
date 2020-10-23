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
unittest for sibilant.compile

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


from functools import partial
from io import StringIO
from types import CodeType, GeneratorType
from unittest import TestCase

import sibilant.builtins

from sibilant.lib import (
    car, cdr, cons, nil, pair, symbol,
    getderef, setderef, clearderef,
)

from sibilant.compiler import (
    is_macro, Macro, is_alias, Alias,
    CompilerException,
)

from . import compile_expr


class Object(object):
    pass


class Defun(TestCase):


    def test_defun(self):
        src = """
        (defun add_8 (x) (+ x 8))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), None)

        add_8 = env["add_8"]
        self.assertTrue(callable(add_8))
        self.assertEqual(add_8.__name__, "add_8")
        self.assertEqual(add_8(1), 9)
        self.assertEqual(add_8(2), 10)

        stmt, env = compile_expr(src, add_8=None)
        self.assertEqual(stmt(), None)

        add_8 = env["add_8"]
        self.assertTrue(callable(add_8))
        self.assertEqual(add_8.__name__, "add_8")
        self.assertEqual(add_8(1), 9)
        self.assertEqual(add_8(2), 10)


class Defmacro(TestCase):


    def test_defmacro(self):
        src = """
        (defmacro swap_test (a b c)
          (cons c b a '()))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), None)

        swap_test = env["swap_test"]
        self.assertTrue(isinstance(swap_test, Macro))
        self.assertTrue(is_macro(swap_test))
        self.assertEqual(swap_test.__name__, "swap_test")

        self.assertRaises(TypeError, swap_test, 1, 2, 3)

        self.assertEqual(swap_test.expand(1, 2, 3),
                         cons(3, 2, 1, nil))

        src = """
        (swap_test 'world 'hello cons)
        """
        # compiles to equivalent of (cons 'hello 'world)
        stmt, env = compile_expr(src, swap_test=swap_test)
        self.assertEqual(stmt(), cons(symbol("hello"), symbol("world")))


class Setf(TestCase):

    def test_setf_var(self):
        src = """
        (begin
          (setf o 101)
          o)
        """
        stmt, env = compile_expr(src, o=100)
        res = stmt()
        self.assertEqual(res, 101)

        src = """
        (begin
          (setf o tacos)
          o)
        """
        stmt, env = compile_expr(src, o=100, tacos=888)
        res = stmt()
        self.assertEqual(res, 888)

        src = """
        (let ((x o))
            (setf x tacos)
            x)
        """
        stmt, env = compile_expr(src, o=100, tacos=777)
        res = stmt()
        self.assertEqual(res, 777)

        src = """
        (begin
          (setf o (i tacos))
          o)
        """
        stmt, env = compile_expr(src, o=100, tacos=888, i=lambda x:x)
        res = stmt()
        self.assertEqual(res, 888)

        src = """
        (let ((x o))
          (setf x (i tacos))
          x)
        """
        stmt, env = compile_expr(src, o=100, tacos=777, i=lambda x:x)
        res = stmt()
        self.assertEqual(res, 777)


    def test_setf_car(self):

        src = """
        (begin
          (setf (car o) 9)
          o)
        """
        stmt, env = compile_expr(src, o=cons(1, 2))
        res = stmt()
        self.assertEqual(res, cons(9, 2))


    def test_setf_cdr(self):

        src = """
        (begin
          (setf (cdr o) 9)
          o)
        """
        stmt, env = compile_expr(src, o=cons(1, 2))
        res = stmt()
        self.assertEqual(res, cons(1, 9))


    def test_setf_item(self):

        src = """
        (begin
          (setf (item o 1) 9)
          o)
        """
        stmt, env = compile_expr(src, o=[1, 2, 3])
        res = stmt()
        self.assertEqual(res, [1, 9, 3])


    def test_setf_member(self):

        o = Object()
        src = """
        (begin
          (setf o.bar tacos)
          o)
        """
        stmt, env = compile_expr(src, o=o, tacos=9)
        res = stmt()
        self.assertEqual(res.bar, 9)

        o = Object()
        o.foo = Object()
        o.foo.bar = Object()
        src = """
        (begin
          (setf o.foo.bar.z tacos)
          o)
        """
        stmt, env = compile_expr(src, o=o, tacos=9)
        res = stmt()
        self.assertEqual(res.foo.bar.z, 9)


    def test_setf_global(self):

        src = """
        (let ((tacos 999))
          (setf (global tacos) 9)
          tacos)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 999)
        self.assertEqual(env["tacos"], 9)

        src = """
        (let ((tacos 999))
          (setf (global tacos) 9)
          tacos)
        """
        stmt, env = compile_expr(src, tacos=5)
        self.assertEqual(stmt(), 999)
        self.assertEqual(env["tacos"], 9)


    def test_setf_item_slice(self):

        seq = [1, 2, 3, 4, 5, 6, 7, 8, 9]

        src = """
        (setf (item-slice seq 0 3) (values 123 456 789))
        """
        stmt, env = compile_expr(src, seq=seq)
        self.assertEqual(stmt(), None)
        self.assertEqual(seq, [123, 456, 789, 4, 5, 6, 7, 8, 9])

        seq = [1, 2, 3, 4, 5, 6, 7, 8, 9]

        src = """
        (setf (item-slice seq step: 3) (values 123 456 789))
        """
        stmt, env = compile_expr(src, seq=seq)
        self.assertEqual(stmt(), None)
        self.assertEqual(seq, [123, 2, 3, 456, 5, 6, 789, 8, 9])


    def test_setf_deref(self):
        src = """
        (let []
          (define X 123)
          (values
             (refq X)
             (lambda [] X)
             (lambda [Y] (setq X Y))))
        """
        stmt, env = compile_expr(src)
        cell, getter, setter = stmt()

        src = """
        (deref cell)
        """
        stmt, env = compile_expr(src, cell=cell)
        self.assertEqual(stmt(), 123)

        src = """
        (setf (deref cell) 456)
        """
        stmt, env = compile_expr(src, cell=cell)
        self.assertEqual(stmt(), None)
        self.assertEqual(getter(), 456)

        src = """
        (delf (deref cell))
        """
        stmt, env = compile_expr(src, cell=cell)
        self.assertEqual(stmt(), None)
        self.assertRaises(NameError, getter)
        self.assertRaises(ValueError, getderef, cell)


class Delf(TestCase):


    def test_delf_item_slice(self):

        seq = [1, 2, 3, 4, 5, 6, 7, 8, 9]

        src = """
        (delf (item-slice seq 0 3))
        """
        stmt, env = compile_expr(src, seq=seq)
        self.assertEqual(stmt(), None)
        self.assertEqual(seq, [4, 5, 6, 7, 8, 9])

        seq = [1, 2, 3, 4, 5, 6, 7, 8, 9]

        src = """
        (delf (item-slice seq step: 3))
        """
        stmt, env = compile_expr(src, seq=seq)
        self.assertEqual(stmt(), None)
        self.assertEqual(seq, [2, 3, 5, 6, 8, 9])


    def test_delf_deref(self):
        src = """
        (let []
          (define X 123)
          (values
             (refq X)
             (lambda [] X)
             (lambda [Y] (setq X Y))))
        """
        stmt, env = compile_expr(src)
        cell, getter, setter = stmt()

        src = """
        (deref cell)
        """
        stmt, env = compile_expr(src, cell=cell)
        self.assertEqual(stmt(), 123)
        self.assertEqual(getter(), 123)
        self.assertEqual(getderef(cell), 123)

        src = """
        (delf (deref cell))
        """
        stmt, env = compile_expr(src, cell=cell)
        self.assertEqual(stmt(), None)
        self.assertRaises(NameError, getter)
        self.assertRaises(ValueError, getderef, cell)


class MacroExpansion(TestCase):

    def test_macro(self):
        src = """
        (let ()
          (def import itertools)
          (define counter (itertools.count))

          (defmacro value++ () (next counter)))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, None)

        value_macro = env["value++"]

        self.assertTrue(is_macro(value_macro))

        src = """
        (values (value++) (value++) (value++))
        """
        stmt, env = compile_expr(src, **env)
        res = stmt()
        self.assertEqual(res, (0, 1, 2))


    def test_alias(self):
        src = """
        (let ()
          (def import itertools)
          (define counter (itertools.count))

          (defalias value++ (next counter)))
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertEqual(res, None)

        value_macro = env["value++"]

        self.assertTrue(is_macro(value_macro))
        self.assertTrue(is_alias(value_macro))

        src = """
        (values value++ value++ value++)
        """
        stmt, env = compile_expr(src, **env)
        res = stmt()
        self.assertEqual(res, (0, 1, 2))


class Eval(TestCase):

    def test_eval_str(self):
        src = """
        (eval "(+ 1 2 tacos)")
        """
        stmt, env = compile_expr(src, tacos=5)
        self.assertEqual(stmt(), 8)


    def test_eval_stream(self):
        data = """
        (+ 1 2 tacos)
        """
        data = StringIO(data)

        src = """
        (eval source_stream)
        """
        stmt, env = compile_expr(src, source_stream=data, tacos=5)
        self.assertEqual(stmt(), 8)


    def test_eval_pair(self):
        data = cons(symbol("+"), 1, 2, symbol("tacos"), nil)

        src = """
        (eval source_obj)
        """
        stmt, env = compile_expr(src, source_obj=data, tacos=5)
        self.assertEqual(stmt(), 8)


    def test_eval_symbol(self):
        data = symbol("tacos")

        src = """
        (eval source_sym)
        """
        stmt, env = compile_expr(src, source_sym=data, tacos=5)
        self.assertEqual(stmt(), 5)


    def test_eval_const(self):
        src = """
        (eval True)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), True)

        src = """
        (eval False)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), False)

        src = """
        (eval 21)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 21)

        src = """
        (eval 2.1)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 2.1)

        src = """
        (eval 2+5i)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), 2+5j)

        src = """
        (eval None)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), None)

        src = """
        (eval ...)
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), ...)

        src = """
        (eval (#tuple))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), ())

        src = """
        (eval (#list))
        """
        stmt, env = compile_expr(src)
        self.assertEqual(stmt(), [])

        src = """
        (eval 2.1d)
        """
        stmt, env = compile_expr(src)
        self.assertRaises(CompilerException, stmt)

        src = """
        (eval 21/10)
        """
        stmt, env = compile_expr(src)
        self.assertRaises(CompilerException, stmt)


    def test_eval_reactivate(self):

        data = []
        work = data.append

        src = """
        (defmacro my_macro [some-arg flag: 'False]
            (define cooked-flag (eval flag))

            (work flag.__class__)
            (work cooked-flag.__class__)
            (if cooked-flag then: "Good" else: "Bad"))
        """
        stmt, env = compile_expr(src, work=work)
        self.assertEqual(stmt(), None)

        self.assertTrue("my_macro" in env)
        my_macro = env["my_macro"]

        src = """
        (my_macro () flag: True)
        """
        stmt, env = compile_expr(src, work=work, my_macro=my_macro)
        self.assertEqual(stmt(), "Good")
        self.assertEqual(data, [symbol, bool])
        data.clear()

        src = """
        (my_macro () flag: (bool 0))
        """
        stmt, env = compile_expr(src, work=work, my_macro=my_macro)
        self.assertEqual(stmt(), "Bad")
        self.assertEqual(data, [pair, bool])


class Compile(TestCase):

    def test_compile_str(self):
        src = """
        (compile "(+ 1 2 tacos)")
        """
        stmt, env = compile_expr(src)
        res = stmt()
        self.assertIs(type(res), CodeType)
        self.assertEqual(eval(res, {"tacos": 5}), 8)


    def test_compile_stream(self):
        data = """
        (+ 1 2 tacos)
        """
        data = StringIO(data)

        src = """
        (compile source_stream)
        """
        stmt, env = compile_expr(src, source_stream=data)
        res = stmt()
        self.assertIs(type(res), CodeType)
        self.assertEqual(eval(res, {"tacos": 5}), 8)


    def test_compile_pair(self):
        data = cons(symbol("+"), 1, 2, symbol("tacos"), nil)

        src = """
        (compile source_obj)
        """
        stmt, env = compile_expr(src, source_obj=data, tacos=5)
        res = stmt()
        self.assertIs(type(res), CodeType)
        self.assertEqual(eval(res, {"tacos": 5}), 8)


    def test_compile_symbol(self):
        data = symbol("tacos")

        src = """
        (compile source_sym)
        """
        stmt, env = compile_expr(src, source_sym=data)
        res = stmt()
        self.assertIs(type(res), CodeType)
        self.assertEqual(eval(res, {"tacos": 5}), 5)


class IterEach(TestCase):

    def test_iter_each(self):
        src = """
        (iter-each [X (range 0 10)]
            (// X 2))
        """
        stmt, env = compile_expr(src)
        res = stmt()

        self.assertEqual(type(res), GeneratorType)
        self.assertEqual(list(res), [0, 0, 1, 1, 2, 2, 3, 3, 4, 4])

        src = """
        (iter-each [X (range 0 10)]
            (// X 2)
            unless: (& X 3))
        """
        stmt, env = compile_expr(src)
        res = stmt()

        self.assertEqual(type(res), GeneratorType)
        self.assertEqual(list(res), [0, 2, 4])

        src = """
        (iter-each [X (range 0 10)]
            (// X 2)
            when: (& X 3))
        """
        stmt, env = compile_expr(src)
        res = stmt()

        self.assertEqual(type(res), GeneratorType)
        self.assertEqual(list(res), [0, 1, 1, 2, 3, 3, 4])

        src = """
        (iter-each [X (range 0 10)]
            (// X 2)
            when: (& X 3) unless: (& X 3))
        """
        stmt, env = compile_expr(src)
        res = stmt()

        self.assertEqual(type(res), GeneratorType)
        self.assertEqual(list(res), [])


    def test_list_each(self):
        src = """
        (list-each [X (range 0 10)]
            (// X 2))
        """
        stmt, env = compile_expr(src)
        res = stmt()

        self.assertEqual(type(res), list)
        self.assertEqual(res, [0, 0, 1, 1, 2, 2, 3, 3, 4, 4])

        src = """
        (list-each [X (range 0 10)]
            (// X 2)
            unless: (& X 3))
        """
        stmt, env = compile_expr(src)
        res = stmt()

        self.assertEqual(type(res), list)
        self.assertEqual(res, [0, 2, 4])

        src = """
        (list-each [X (range 0 10)]
            (// X 2)
            when: (& X 3))
        """
        stmt, env = compile_expr(src)
        res = stmt()

        self.assertEqual(type(res), list)
        self.assertEqual(res, [0, 1, 1, 2, 3, 3, 4])

        src = """
        (list-each [X (range 0 10)]
            (// X 2)
            when: (& X 3) unless: (& X 3))
        """
        stmt, env = compile_expr(src)
        res = stmt()

        self.assertEqual(type(res), list)
        self.assertEqual(res, [])


    def test_set_each(self):
        src = """
        (set-each [X (range 0 10)]
            (// X 2))
        """
        stmt, env = compile_expr(src)
        res = stmt()

        self.assertEqual(type(res), set)
        self.assertEqual(res, {0, 1, 2, 3, 4})

        src = """
        (set-each [X (range 0 10)]
            (// X 2)
            unless: (& X 3))
        """
        stmt, env = compile_expr(src)
        res = stmt()

        self.assertEqual(type(res), set)
        self.assertEqual(res, {0, 2, 4})

        src = """
        (set-each [X (range 0 10)]
            (// X 2)
            when: (& X 3))
        """
        stmt, env = compile_expr(src)
        res = stmt()

        self.assertEqual(type(res), set)
        self.assertEqual(res, {0, 1, 2, 3, 4})

        src = """
        (set-each [X (range 0 10)]
            (// X 2)
            when: (& X 3) unless: (& X 3))
        """
        stmt, env = compile_expr(src)
        res = stmt()

        self.assertEqual(type(res), set)
        self.assertEqual(res, set())


class Lets(TestCase):


    def test_let_star(self):
        src = """
        (let* [[pre "te"][post (+ pre "st")]] post)
        """
        stmt, env = compile_expr(src)
        res = stmt()

        self.assertEqual(res, "test")

        src = """
        (let* [[post (+ pre "st")][pre "te"]] post)
        """
        stmt, env = compile_expr(src)

        self.assertRaises(NameError, stmt)


    def test_let_star_values(self):
        src = """
        (let*-values [[(a (b c)) (#tuple 1 (#tuple 2 3))]
                      [(d e) `(,(+ a b) ,(+ a c))]]
                     `(,d . ,e))
        """
        stmt, env = compile_expr(src)
        res = stmt()

        self.assertEqual(res, cons(3, 4, nil))

        src = """
        (let*-values [[(d e) `(,(+ a b) ,(+ a c))]
                      [(a (b c)) (#tuple 1 (#tuple 2 3))]]
                     `(,d . ,e))
        """
        stmt, env = compile_expr(src)

        self.assertRaises(NameError, stmt)


    def test_letrec(self):
        src = """
        (letrec [[is-even? (lambda (n) (or (== n 0)
                                       (is-odd? (- n 1))))]
                 [is-odd? (lambda (n) (and (not (== n 0))
                                           (is-even? (- n 1))))]]
                (is-odd? 11))
        """
        stmt, env = compile_expr(src)
        res = stmt()

        self.assertEqual(res, True)


class Attrs(TestCase):


    def test_has_attr(self):
        src = """
        (has-attr None __class__)
        """
        stmt, env = compile_expr(src)
        res = stmt()

        self.assertEqual(res, True)

        src = """
        (has-attr None "__class__")
        """
        stmt, env = compile_expr(src)
        res = stmt()

        self.assertEqual(res, True)


class Compose(TestCase):


    def test_compose_fn(self):

        src = """
        (compose
        (partial + 1)
        (partial * 2)
        float
        str)
        """

        stmt, env = compile_expr(src)
        res = stmt()

        self.assertTrue(callable(res))
        res = res(101)

        self.assertEqual(res, "204.0")


    def test_compose_lambda(self):
        src = """
        (compose
        (lambda [x]
        (! append col x)
        (+ x 1))
        (lambda [x]
        (! append col x)
        (* x 2))
        (lambda [x]
        (! append col x)
        x)
        float
        (lambda [x]
        (! append col x)
        x)
        str)
        """

        col = []
        stmt, env = compile_expr(src, col=col)
        res = stmt()

        self.assertTrue(callable(res))
        res = res(101)

        self.assertEqual(col, [101, 102, 204, 204.0])
        self.assertIs(type(col[-1]), float)
        self.assertIs(type(res), str)
        self.assertEqual(res, "204.0")


#
# The end.
