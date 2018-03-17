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
unit tests for sibilant core types

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


from functools import partial
from unittest import TestCase

from sibilant import (
    cons, pair, nil, is_pair, is_proper, is_nil,
    car, cdr, setcar, setcdr, last,
    symbol, is_symbol, keyword, is_keyword,
    build_unpack_pair, values,
)


# this name is too long.
b_u_p = build_unpack_pair


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

        self.assertNotEqual(u, None)
        self.assertNotEqual(u, nil)
        self.assertNotEqual(u, cons(1, nil))
        self.assertNotEqual(u, cons(1, 3))
        self.assertNotEqual(u, cons(1, 3, nil))


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

        self.assertNotEqual(z, None)
        self.assertNotEqual(z, nil)
        self.assertNotEqual(z, cons(1, nil))
        self.assertNotEqual(z, cons(1, 3))
        self.assertNotEqual(z, cons(1, 3, nil))


    def test_nil(self):
        # singleton nil check
        Nil = type(nil)

        self.assertEqual(id(nil), id(Nil()))
        self.assertEqual(id(Nil()), id(Nil()))
        self.assertTrue(nil is Nil())

        # behavior
        self.assertIsInstance(nil, pair)
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

        b = pair(0, a)
        c = pair(0, a)
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

        self.assertEqual(str(a), "(1 2 . (3 ...))")
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
        self.assertIsInstance(x, symbol)

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
        self.assertIsInstance(x, keyword)

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


class BuildUnpackPairTest(TestCase):

    def test_build_unpack_pair(self):
        # a bunch of simple expected inputs and outputs for
        # build_unpack_pair, including empties and impropers

        self.assertEqual(b_u_p([1, 2, 3]),
                         cons(1, 2, 3))

        self.assertEqual(b_u_p([1, 2, 3], []),
                         cons(1, 2, 3))

        self.assertEqual(b_u_p([1, 2, 3], [4]),
                         cons(1, 2, 3, 4))

        self.assertEqual(b_u_p([1, 2, 3], [], [4]),
                         cons(1, 2, 3, 4))

        self.assertEqual(b_u_p([1, 2, 3], [], [1, 2]),
                         cons(1, 2, 3, 1, 2))

        self.assertEqual(b_u_p(cons(1, nil), nil, [1, 2]),
                         cons(1, 1, 2))

        self.assertEqual(b_u_p([1, 2, 3], nil),
                         cons(1, 2, 3, nil))

        self.assertEqual(b_u_p([1, 2, 3], [nil]),
                         cons(1, 2, 3, nil))

        self.assertEqual(b_u_p([1, 2, 3], [], nil),
                         cons(1, 2, 3, nil))

        # a nil as an item, as opposed to as a sequence itself, should
        # end up in the output
        self.assertEqual(b_u_p([1, 2, 3], [nil], nil),
                         cons(1, 2, 3, nil, nil))

        self.assertEqual(b_u_p([1, 2, 3], [], cons(4, nil)),
                         cons(1, 2, 3, 4, nil))

        self.assertEqual(b_u_p([1]),
                         cons(1))

        self.assertEqual(b_u_p(cons(1, 2), cons(3, 4)),
                         cons(1, 2, 3, 4))

        self.assertEqual(b_u_p(cons(1, 2, nil), cons(3, 4)),
                         cons(1, 2, 3, 4))

        self.assertEqual(b_u_p(cons(1, 2, nil), cons(3, 4, nil)),
                         cons(1, 2, 3, 4, nil))

        self.assertEqual(b_u_p(cons(1, 2), cons(3, 4, nil)),
                         cons(1, 2, 3, 4, nil))

        self.assertEqual(b_u_p(cons(1, 2)),
                         cons(1, 2))

        self.assertEqual(b_u_p([1, 2, 3], [], cons(1, 2)),
                         cons(1, 2, 3, 1, 2))

        self.assertEqual(b_u_p(cons(1, 2, nil), nil),
                         cons(1, 2, nil))

        self.assertEqual(b_u_p(cons(1, 2), nil),
                         cons(1, 2, nil))

        self.assertEqual(b_u_p(nil, cons(1, 2)),
                         cons(1, 2))

        self.assertEqual(b_u_p(), nil)
        self.assertEqual(b_u_p([]), nil)
        self.assertEqual(b_u_p(nil), nil)
        self.assertEqual(b_u_p([], nil), nil)
        self.assertEqual(b_u_p(nil, []), nil)


    def test_non_iterable(self):
        # attempting to iterate over a non-iterable
        self.assertRaises(TypeError, b_u_p, 5, [1])
        self.assertRaises(TypeError, b_u_p, [1], 5)


    def test_good_iterable(self):
        # an iterable is fine
        self.assertEqual(b_u_p(range(0, 0)), nil)
        self.assertEqual(b_u_p(range(0, 3)), cons(0, 1, 2))
        self.assertEqual(b_u_p(range(0, 3), nil), cons(0, 1, 2, nil))
        self.assertEqual(b_u_p(range(0, 3), [3]), cons(0, 1, 2, 3))
        self.assertEqual(b_u_p(range(0, 3), [3, nil]), cons(0, 1, 2, 3, nil))
        self.assertEqual(b_u_p([0], range(1, 4)), cons(0, 1, 2, 3))
        self.assertEqual(b_u_p([0], range(1, 4), nil), cons(0, 1, 2, 3, nil))


    def test_raise_iterable(self):
        class OhNoes(Exception):
            pass

        def iter_fail(when):
            yield from range(0, when)
            raise OhNoes()

        # if an iterable raises part-way through, let's make sure that
        # there isn't a segfault or something, and that the exception
        # is correctly propagated up
        self.assertRaises(OhNoes, b_u_p, iter_fail(3))
        self.assertRaises(OhNoes, b_u_p, [1], iter_fail(3))
        self.assertRaises(OhNoes, b_u_p, iter_fail(3), [1])


class ValuesTest(TestCase):


    def test_equality(self):
        """
        tests to ensure the equality operations of a values are
        functioning correctly when compared with another values
        instance
        """

        # permutations of values with only args
        a = values(1, 2, 3)
        b = values(1, 2, 3)
        c = values(4, 5, 6)

        self.assertEqual(a, a)
        self.assertEqual(a, b)
        self.assertEqual(b, a)
        self.assertNotEqual(a, c)
        self.assertNotEqual(b, c)

        # permutations of values with only kwds
        d = values(foo=9, bar=10)
        e = values(foo=9, bar=10)
        f = values(foo=100, quuz=200)

        self.assertEqual(d, d)
        self.assertEqual(d, e)
        self.assertEqual(e, d)
        self.assertNotEqual(d, f)
        self.assertNotEqual(f, d)

        self.assertNotEqual(a, d)
        self.assertNotEqual(d, a)

        # permutations of values with both args and kwds
        g = values(1, 2, 3, foo=9, bar=10)
        h = values(1, 2, 3, foo=9, bar=10)
        i = values(1, 2, 3, foo=100, quuz=200)
        j = values(4, 5, 6, foo=9, bar=10)
        k = values(4, 5, 6, foo=100, quuz=200)

        self.assertEqual(g, g)  # identity
        self.assertEqual(g, h)  # similar
        self.assertEqual(h, g)  # - reversed
        self.assertNotEqual(a, g)  # args vs. args and kwds
        self.assertNotEqual(g, a)  # args and kwds vs. args
        self.assertNotEqual(d, g)  # kwds vs. args and kwds
        self.assertNotEqual(g, d)  # args and kwds vs. kwds
        self.assertNotEqual(g, i)  # args same, kwds differ
        self.assertNotEqual(i, g)  # - reversed
        self.assertNotEqual(g, j)  # args differ, kwds same
        self.assertNotEqual(j, g)  # - reversed
        self.assertNotEqual(g, k)  # ares and kwds differ
        self.assertNotEqual(k, g)  # - reversed

        # the empty values, test it against the non-empty ones to make
        # sure nothing breaks
        z = values()

        self.assertNotEqual(a, z)
        self.assertNotEqual(z, a)
        self.assertNotEqual(d, z)
        self.assertNotEqual(z, d)
        self.assertNotEqual(g, z)
        self.assertNotEqual(z, g)


    def test_sequence(self):
        """
        tests treating the values object as a sequence, including equality
        operations against other sequences
        """

        a = values(1, 2, 3)
        b = values(4, 5, 6)

        self.assertEqual(sum(a), 6)
        self.assertEqual(sum(b), 15)

        self.assertEqual(list(a), [1, 2, 3])
        self.assertEqual(list(b), [4, 5, 6])

        self.assertEqual(a, (1, 2, 3))
        self.assertEqual((1, 2, 3), a)
        self.assertEqual(b, (4, 5, 6))
        self.assertEqual((4, 5, 6), b)

        self.assertNotEqual(a, (4, 5, 6))
        self.assertNotEqual((4, 5, 6), a)

        self.assertNotEqual(b, (1, 2, 3))
        self.assertNotEqual((1, 2, 3), b)

        c = values(1, 2, 3, foo=5)

        self.assertEqual(list(c), [1, 2, 3])

        self.assertNotEqual(c, (1, 2, 3))
        self.assertNotEqual((1, 2, 3), c)


    def test_mapping(self):
        """
        tests treating the values object as a mapping, including equality
        operations against other mappings
        """

        a = values(a=1, b=2, c=3)
        b = values(a=4, b=5, c=6)

        self.assertEqual(dict(a), {'a': 1, 'b': 2, 'c': 3})
        self.assertEqual(dict(b), {'a': 4, 'b': 5, 'c': 6})

        # equality works in both directions
        self.assertEqual(a, {'a': 1, 'b': 2, 'c': 3})
        self.assertEqual({'a': 1, 'b': 2, 'c': 3}, a)
        self.assertEqual(b, {'a': 4, 'b': 5, 'c': 6})
        self.assertEqual({'a': 4, 'b': 5, 'c': 6}, b)

        # so does inequality
        self.assertNotEqual(a, {'a': 4, 'b': 5, 'c': 6})
        self.assertNotEqual({'a': 4, 'b': 5, 'c': 6}, a)

        self.assertNotEqual(b, {'a': 1, 'b': 2, 'c': 3})
        self.assertNotEqual({'a': 1, 'b': 2, 'c': 3}, b)

        c = values(1, 2, 3, a=4, b=5, c=6)

        # whether it has positionals or not, only the keywords get
        # converted over this way
        self.assertEqual(dict(c), {'a': 4, 'b': 5, 'c': 6})

        # a values with positionals can never be equivalent to a map
        self.assertNotEqual(c, {'a': 4, 'b': 5, 'c': 6})
        self.assertNotEqual({'a': 4, 'b': 5, 'c': 6}, c)


    def test_subscript(self):
        """
        tests that subscripting will defer correctly between positional
        and keyword members
        """

        a = values(1, 2, 3, a=4, b=5)

        self.assertEqual(a[0], 1)
        self.assertEqual(a[1], 2)
        self.assertEqual(a[2], 3)

        self.assertEqual(a['a'], 4)
        self.assertEqual(a['b'], 5)

        self.assertRaises(IndexError, lambda: a[3])
        self.assertRaises(KeyError, lambda: a['c'])

        b = values(1, 2, 3)
        self.assertRaises(KeyError, lambda: b['a'])


    def test_invoke(self):
        """
        tests that invocation functions as expected, and raises the
        appropriate TypeErrors when there's a mismatch between the
        values members and the function signature

        todo: currently only unary invocation works. ie. you have to
        pass only one argument to a values instance, and that must be
        a callable that has a signature that works with the contents
        of the values. Later on, I'll add support for passing
        additional arguments along such that positional arguments are
        appended to the values' positionals, and keyword arguments are
        merged (with the invocation ones winning)
        """

        def gather(a, b, c, d=0):
            return [a, b, c, d]

        v = values(1, 2, 3)
        self.assertEqual(v(gather), [1, 2, 3, 0])

        v = values(1, 2, 3, 4)
        self.assertEqual(v(gather), [1, 2, 3, 4])

        v = values(1, 2, 3, d=9)
        self.assertEqual(v(gather), [1, 2, 3, 9])

        v = values(c=8, b=7, a=6)
        self.assertEqual(v(gather), [6, 7, 8, 0])

        v = values(d=9, c=8, b=7, a=6)
        self.assertEqual(v(gather), [6, 7, 8, 9])

        v = values()
        self.assertRaises(TypeError, v, gather)

        v = values(d=5)
        self.assertRaises(TypeError, v, gather)

        v = values(1, 2)
        self.assertRaises(TypeError, v, gather)

        v = values(1, 2, d=5)
        self.assertRaises(TypeError, v, gather)

        v = values(1, 2, 3, 4, 5)
        self.assertRaises(TypeError, v, gather)

        v = values(1, 2, 3, foo=100)
        self.assertRaises(TypeError, v, gather)


    def test_copy(self):

        a = values(1, 2, 3)
        b = values(foo=4, bar=5)
        c = values(1, 2, 3, foo=4, bar=5)

        d = a(values)
        e = b(values)
        f = c(values)

        self.assertEqual(a, d)
        self.assertEqual(b, e)
        self.assertEqual(c, f)


    def test_repr(self):
        """
        tests that the repr of a values is as expected.

        todo: this repr is short-term, because it was the easiest to
        write. I'll reimplement it at some point so that it looks more
        like the actual invocation. For now this is good enough.
        """

        a = values()
        self.assertEqual(repr(a), "values()")

        a = values(1, 2, 3)
        self.assertEqual(repr(a), "values(*(1, 2, 3))")

        a = values(foo=4)
        self.assertEqual(repr(a), "values(**{'foo': 4})")

        a = values(1, 2, 3, foo=4)
        self.assertEqual(repr(a), "values(*(1, 2, 3), **{'foo': 4})")


#
# The end.
