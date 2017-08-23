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
unit tests for sibilant

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


from functools import partial
from unittest import TestCase

from sibilant import (
    cons, nil, is_pair, is_proper, is_nil,
    car, cdr, setcar, setcdr, last,
    symbol, is_symbol, keyword, is_keyword,
    Pair, Nil, Symbol, Keyword,
)


class ConsTest(TestCase):

    def test_proper_cons(self):
        a = cons(2, nil)
        b = cons(1, a)
        c = cons(0, b)

        self.assertEqual(a.count(), 1)
        self.assertEqual(b.count(), 2)
        self.assertEqual(c.count(), 3)

        self.assertTrue(a.is_proper())
        self.assertTrue(b.is_proper())
        self.assertTrue(c.is_proper())

        self.assertFalse(a.is_recursive())
        self.assertFalse(b.is_recursive())
        self.assertFalse(c.is_recursive())

        self.assertEqual(car(b), 1)
        self.assertEqual(car(cdr(b)), 2)

        self.assertEqual(car(c), 0)
        self.assertEqual(car(cdr(c)), 1)
        self.assertEqual(car(cdr(cdr(c))), 2)

        x, (y, (z, n)) = c
        self.assertEqual(x, 0)
        self.assertEqual(y, 1)
        self.assertEqual(z, 2)
        self.assertEqual(n, nil)

        x, y, z = c.unpack()
        self.assertEqual(x, 0)
        self.assertEqual(y, 1)
        self.assertEqual(z, 2)

        self.assertEqual(c.count(), 3)

        self.assertSequenceEqual(list(c.unpack()), [0, 1, 2])
        self.assertSequenceEqual(tuple(c.unpack()), (0, 1, 2))
        self.assertEqual(str(c), "(0 1 2)")
        self.assertEqual(repr(c), "cons(0, 1, 2, nil)")

        self.assertEqual(car(a), 2)
        self.assertEqual(cdr(a), nil)

        self.assertEqual(car(b), 1)
        self.assertEqual(cdr(b), a)

        w = cons(0, cons(1, cons(2, nil)))
        self.assertEqual(c, w)
        self.assertEqual(w, c)

        u = cons(99, c)
        v = cons(99, w)
        self.assertEqual(u, v)
        self.assertEqual(v, u)

        self.assertNotEqual(cons(99, nil), u)
        self.assertNotEqual(u, cons(99, nil))


    def test_improper_cons(self):
        z = cons(1, 2)
        self.assertEqual(z.count(), 2)
        self.assertFalse(z.is_proper())
        self.assertEqual(car(z), 1)
        self.assertEqual(cdr(z), 2)
        self.assertEqual(str(z), "(1 . 2)")
        self.assertEqual(repr(z), "cons(1, 2)")

        self.assertTrue(is_pair(z))
        self.assertFalse(is_proper(z))


    def test_nil(self):
        # singleton nil check
        self.assertEqual(id(nil), id(Nil()))
        self.assertEqual(id(Nil()), id(Nil()))
        self.assertTrue(nil is Nil())

        # behavior
        self.assertIsInstance(nil, Pair)
        self.assertTrue(is_pair(nil))
        self.assertTrue(is_nil(nil))
        self.assertTrue(is_proper(nil))
        self.assertFalse(nil)
        self.assertEqual(str(nil), "nil")
        self.assertEqual(repr(nil), "nil")

        self.assertEqual(nil, nil)
        self.assertNotEqual(nil, cons(1, nil))
        self.assertNotEqual(cons(1, nil), nil)

        with self.assertRaises(TypeError):
            car(nil)

        with self.assertRaises(TypeError):
            cdr(nil)

        self.assertEqual(list(nil), list())
        self.assertEqual(tuple(nil), tuple())


    def test_recursive_cons(self):
        a = cons(1, 2, 3, recursive=True)

        self.assertTrue(a.is_proper())
        self.assertTrue(a.is_recursive())

        self.assertTrue(is_pair(a))
        self.assertTrue(is_proper(a))

        self.assertEqual(a.count(), 3)

        self.assertEqual(car(a), car(cdr(cdr(cdr(a)))))

        self.assertEqual(str(a), "(1 2 3 ...)")
        self.assertEqual(repr(a), "cons(1, 2, 3, recursive=True)")

        b = Pair(0, a)
        c = Pair(0, a)
        self.assertEqual(b, c)
        self.assertNotEqual(a, b)
        self.assertNotEqual(a, c)

        z = cons(1, cons(2, cons(3, nil)))
        setcdr(cdr(cdr(z)), z)

        self.assertEqual(a, z)
        self.assertEqual(z, a)


    def test_recursive_tail_cons(self):
        a = cons(1, 2, cons(3, recursive=True))

        self.assertTrue(a.is_proper())
        self.assertTrue(a.is_recursive())

        self.assertEqual(a.count(), 3)

        self.assertEqual(car(cdr(cdr(cdr(a)))),
                         car((cdr(cdr(cdr(cdr(a)))))))

        # TODO: is this really how scheme would print this, when only
        # the 3 would repeat?
        self.assertEqual(str(a), "(1 2 3 ...)")
        self.assertEqual(repr(a), "cons(1, 2, cons(3, recursive=True))")

        b = cons(0, a)
        c = cons(0, a)
        self.assertEqual(b, c)
        self.assertNotEqual(a, b)
        self.assertNotEqual(a, c)

        z = cons(3, nil)
        setcdr(z, z)
        z = cons(1, cons(2, z))

        self.assertEqual(a, z)
        self.assertEqual(z, a)


class SymbolTest(TestCase):

    def test_symbol(self):
        x = symbol('x')
        y = symbol('x')
        z = symbol(x)

        self.assertTrue(is_symbol(x))
        self.assertIsInstance(x, Symbol)

        self.assertEqual(x, x)
        self.assertEqual(x, y)
        self.assertEqual(x, z)
        self.assertEqual(y, x)
        self.assertEqual(y, y)
        self.assertEqual(y, z)
        self.assertEqual(z, x)
        self.assertEqual(z, y)
        self.assertEqual(z, z)

        self.assertEqual(id(x), id(y))
        self.assertEqual(id(y), id(z))

        self.assertTrue(x is y)
        self.assertTrue(y is z)

        w = symbol('w')

        self.assertNotEqual(x, w)
        self.assertNotEqual(w, x)
        self.assertFalse(x is w)


    def test_symbol_2(self):
        w = "X"
        x = symbol(w)
        y = symbol(str(x))
        z = symbol(str(y))

        self.assertEqual(x, x)
        self.assertEqual(x, y)
        self.assertEqual(x, z)
        self.assertEqual(y, x)
        self.assertEqual(y, y)
        self.assertEqual(y, z)
        self.assertEqual(z, x)
        self.assertEqual(z, y)
        self.assertEqual(z, z)

        self.assertEqual(id(x), id(y))
        self.assertEqual(id(y), id(z))

        self.assertTrue(x is y)
        self.assertTrue(y is z)

        self.assertEqual(id(w), id(str(x)))
        self.assertEqual(id(w), id(str(y)))
        self.assertEqual(id(w), id(str(z)))


    def test_dict(self):
        x = symbol('x')
        y = symbol('y')

        d = dict()
        d[x] = "cookies"
        d[y] = "cake"
        d['x'] = "chicken"
        d['y'] = "tuna"

        self.assertEqual(d[x], "cookies")
        self.assertEqual(d[y], "cake")
        self.assertEqual(d['x'], "chicken")
        self.assertEqual(d['y'], "tuna")


    def test_repr_str(self):
        x = symbol('x')
        self.assertEqual(repr(x), "<symbol 'x'>")
        self.assertEqual(str(x), 'x')


    def test_against_str(self):
        x = symbol('x')

        self.assertFalse(x is 'x')

        self.assertNotEqual(x, 'x')
        self.assertNotEqual('x', x)


class KeywordTest(TestCase):

    def test_keyword(self):
        x = keyword('x')
        y = keyword('x')
        z = keyword(x)

        self.assertTrue(is_keyword(x))
        self.assertIsInstance(x, Keyword)

        self.assertEqual(x, x)
        self.assertEqual(x, y)
        self.assertEqual(x, z)
        self.assertEqual(y, x)
        self.assertEqual(y, y)
        self.assertEqual(y, z)
        self.assertEqual(z, x)
        self.assertEqual(z, y)
        self.assertEqual(z, z)

        self.assertEqual(id(x), id(y))
        self.assertEqual(id(y), id(z))

        self.assertTrue(x is y)
        self.assertTrue(y is z)

        w = keyword('w')

        self.assertNotEqual(x, w)
        self.assertNotEqual(w, x)
        self.assertFalse(x is w)


    def test_keyword_2(self):
        w = "X"
        x = keyword(w)
        y = keyword(str(x))
        z = keyword(str(y))

        self.assertEqual(x, x)
        self.assertEqual(x, y)
        self.assertEqual(x, z)
        self.assertEqual(y, x)
        self.assertEqual(y, y)
        self.assertEqual(y, z)
        self.assertEqual(z, x)
        self.assertEqual(z, y)
        self.assertEqual(z, z)

        self.assertEqual(id(x), id(y))
        self.assertEqual(id(y), id(z))

        self.assertTrue(x is y)
        self.assertTrue(y is z)

        self.assertEqual(id(w), id(str(x)))
        self.assertEqual(id(w), id(str(y)))
        self.assertEqual(id(w), id(str(z)))


    def test_dict(self):
        x = keyword('x')
        y = keyword('y')

        d = dict()
        d[x] = "cookies"
        d[y] = "cake"
        d['x'] = "chicken"
        d['y'] = "tuna"

        self.assertEqual(d[x], "cookies")
        self.assertEqual(d[y], "cake")
        self.assertEqual(d['x'], "chicken")
        self.assertEqual(d['y'], "tuna")


    def test_against_symbol(self):
        xk = keyword("x")
        xs = symbol("x")

        self.assertFalse(xk is xs)
        self.assertNotEqual(xk, xs)
        self.assertNotEqual(xs, xk)


    def test_repr_str(self):
        x = keyword('x')
        self.assertEqual(repr(x), "<keyword 'x'>")
        self.assertEqual(str(x), 'x')


    def test_against_str(self):
        x = keyword('x')

        self.assertFalse(x is 'x')

        self.assertNotEqual(x, 'x')
        self.assertNotEqual('x', x)


#
# The end.
