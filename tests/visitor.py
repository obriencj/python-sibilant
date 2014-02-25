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
unittest for sibilant.visitor

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


from sibilant.visitor import Visitor, NoVisitMethod
from unittest import TestCase


class Foo(object):
    pass


class Bar(Foo):
    pass


class Baz(Foo):
    pass


class Qux(object):
    pass


class V(Visitor):

    def visitFoo(self, o):
        return "foo"

    def visitBar(self, o):
        return "bar"


class VisitorTest(TestCase):


    def test_visit(self):
        v = V()

        foo = Foo()
        bar = Bar()
        baz = Baz()
        qux = Qux()

        self.assertEqual(v.visit(foo), "foo")
        self.assertEqual(v.visit(bar), "bar")
        self.assertEqual(v.visit(baz), "foo")

        with self.assertRaises(NoVisitMethod):
            v.visit(qux)


#
# The end.
