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


from unittest import TestCase
from sibilant import cons, nil, niltype, last
from sibilant import car, cdr
from sibilant import symbol
from sibilant import ref, attr, undefined, deref, setref


class ConsTest(TestCase):

    def test_proper_cons(self):
        a = cons(2, nil)
        b = cons(1, a)
        c = cons(0, b)

        self.assertTrue(a.is_proper())
        self.assertTrue(b.is_proper())
        self.assertTrue(c.is_proper())

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
        self.assertEqual(repr(c), "cons(0, cons(1, cons(2, niltype())))")

        self.assertEqual(car(a), 2)
        self.assertEqual(cdr(a), nil)

        self.assertEqual(car(b), 1)
        self.assertEqual(cdr(b), a)


    def test_improper_cons(self):
        z = cons(1, 2)
        self.assertFalse(z.is_proper())
        self.assertEqual(car(z), 1)
        self.assertEqual(cdr(z), 2)
        self.assertEqual(str(z), "(1 . 2)")


    def test_nil(self):
        # singleton nil check
        self.assertEqual(id(nil), id(niltype()))
        self.assertEqual(id(niltype()), id(niltype()))
        self.assertTrue(nil is niltype())

        # behavior
        self.assertIsInstance(nil, cons)
        self.assertFalse(nil)
        self.assertEqual(str(nil), "()")
        self.assertEqual(repr(nil), "niltype()")

        with self.assertRaises(TypeError):
            car(nil)

        with self.assertRaises(TypeError):
            cdr(nil)

        self.assertEqual(list(nil), list())
        self.assertEqual(tuple(nil), tuple())


class SymbolTest(TestCase):

    def test_symbol(self):
        x = symbol('x')
        y = symbol('x')
        z = symbol(x)

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
        self.assertEqual(repr(x), "symbol('x')")
        self.assertEqual(str(x), 'x')


    def test_against_str(self):
        x = symbol('x')

        self.assertFalse(x is 'x')

        self.assertNotEqual(x, 'x')
        self.assertNotEqual('x', x)


class RefTest(TestCase):

    def test_ref(self):
        a = ref(symbol('a'))
        self.assertEqual(deref(a), undefined)
        self.assertEqual(setref(a, 100), 100)
        self.assertEqual(deref(a), 100)

        self.assertEqual(repr(a), "ref(symbol('a'))")

        b = ref(symbol('b'), 101)
        self.assertEqual(deref(b), 101)


    def test_attr(self):
        a = ref(symbol('a'))
        b = attr(symbol('b'), a._get_value, a._set_value)

        self.assertEqual(deref(a), deref(b))
        self.assertEqual(deref(b), undefined)

        self.assertEqual(setref(b, 101), 101)
        self.assertEqual(deref(b), 101)
        self.assertEqual(deref(a), 101)

        self.assertEqual(setref(a, 102), 102)
        self.assertEqual(deref(b), 102)
        self.assertEqual(deref(a), 102)

        c = attr(symbol('c'), a._get_value)

        self.assertEqual(deref(c), 102)

        with self.assertRaises(AttributeError):
            setref(c, 999)


#
# The end.
