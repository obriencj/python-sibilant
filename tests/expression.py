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


from sibilant.ast import compose_from_str
from sibilant.expression import *
from unittest import TestCase


def compose_expr(src_str):
    node = compose_from_str(src_str)
    return translate_node(node)


class TestEval(TestCase):

    def test_special_apply(self):
        src = "(testing for fun)"
        col = compose_expr(src)
        exp = Apply((1, 0),
                    Variable((1, 1), "testing"),
                    Variable((1, 9), "for"),
                    Variable((1, 13), "fun"))

        self.assertEqual(col, exp)
        self.assertEqual(repr(col), repr(exp))


    def test_special_begin(self):
        src = "(begin to dance)"
        col = compose_expr(src)
        exp = Begin((1, 0),
                    Variable((1, 7), "to"),
                    Variable((1, 10), "dance"))

        self.assertEqual(col, exp)
        self.assertEqual(repr(col), repr(exp))

        src = "(begin (to dance))"
        col = compose_expr(src)
        exp = Begin((1, 0),
                    Apply((1, 7),
                          Variable((1, 8), "to"),
                          Variable((1, 11), "dance")))

        self.assertEqual(col, exp)
        self.assertEqual(repr(col), repr(exp))


    def test_special_lambda(self):
        src = "(lambda (i) i)"

        col = compose_expr(src)
        exp = Lambda((1, 0),
                     FormalsList((1, 8),
                          Formal((1, 9), "i")),
                     Variable((1, 12), "i"))

        self.assertEqual(col, exp)
        self.assertEqual(repr(col), repr(exp))

        src = """
        (lambda (x)
            (lambda (y) (add x y)))
        """
        col = compose_expr(src)
        exp = Lambda((2, 8),
                     FormalsList((2, 16),
                                 Formal((2, 17), "x")),
                     Lambda((3, 12),
                            FormalsList((3, 20),
                                        Formal((3, 21), "y")),
                            Apply((3, 24),
                                  Variable((3, 25), "add"),
                                  Variable((3, 29), "x"),
                                  Variable((3, 31), "y"))))

        self.assertEqual(col, exp)
        self.assertEqual(repr(col), repr(exp))


#
# The end.
