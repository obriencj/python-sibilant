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
Unit tests for sibilant.eval

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


from unittest import TestCase


class TestEval(object):
    # temporarily not a TestCase, still writing it

    def test_special_apply(self):
        src = "(testing for fun)"
        col = compose_ast(src)
        exp = Apply(1,
                    Symbol(1, "testing"),
                    Symbol(1, "for"),
                    Symbol(1, "fun"))

        self.assertEqual(col, exp)


    def test_special_begin(self):
        src = "(begin to dance)"
        col = compose_ast(src)
        exp = Begin(1,
                    Symbol(1, "to"),
                    Symbol(1, "dance"))

        self.assertEqual(col, exp)

        src = "(begin (to dance))"
        col = compose_ast(src)
        exp = Begin(1,
                    Apply(1,
                          Symbol(1, "to"),
                          Symbol(1, "dance")))

        self.assertEqual(col, exp)


    def test_special_lambda(self):
        src = "(lambda (i) i)"

        col = compose_ast(src)
        exp = Lambda(1,
                     List(1,
                          Symbol(1, "i")),
                     Symbol(1, "i"))

        self.assertEqual(col, exp)

        src = """
        (lambda (x)
            (lambda (y) (add x y)))
        """
        col = compose_ast(src)
        exp = Lambda(2,
                     List(2,
                          Symbol(2, "x")),
                     Lambda(3,
                            List(3,
                                 Symbol(3, "y")),
                            Apply(3,
                                  Symbol(3, "add"),
                                  Symbol(3, "x"),
                                  Symbol(3, "y"))))

        self.assertEqual(col, exp)


#
# The end.
