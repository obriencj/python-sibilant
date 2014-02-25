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
unittest for sibilant.dispatch

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


from sibilant.dispatch import Dispatch, NoDispatchMethod
from unittest import TestCase


class Foo(object):
    pass


class Bar(Foo):
    pass


class Baz(Foo):
    pass


class Qux(object):
    pass


class D(Dispatch):

    def dispatchFoo(self, o):
        return "foo"

    def dispatchBar(self, o):
        return "bar"


class DispatchTest(TestCase):


    def test_dispatch(self):
        dis = D().dispatch

        foo = Foo()
        bar = Bar()
        baz = Baz()
        qux = Qux()

        self.assertEqual(dis(foo), "foo")
        self.assertEqual(dis(bar), "bar")
        self.assertEqual(dis(baz), "foo")

        with self.assertRaises(NoDispatchMethod):
            dis(qux)


#
# The end.
