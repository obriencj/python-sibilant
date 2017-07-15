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


from unittest import TestCase

from sibilant import car, cdr, cons, nil, symbol
from sibilant.module import module


def getter_setter(value):
    def getter():
        return value
    def setter(v):
        nonlocal value
        value = v
    return getter, setter


result = None

def set_result(value):
    global result
    result = value

# exposed in the created module so we can get state back out.
defaults = {"set-result": set_result}


mod_source = """
(define tacos 5)
(define beer 1)

(define make_adder
  (lambda (by) (lambda (y) (+ y by))))

(let ((add-8 (make_adder 8)))
  (set_result (add-8 100)))
"""


class ModuleTest(TestCase):

    def test_1(self):
        getter, setter = getter_setter(None)

        defaults = {"set_result": setter}
        test_module = module("test_module", mod_source, defaults=defaults)

        self.assertEqual(getter(), 108)
        self.assertEqual(test_module.tacos, 5)
        self.assertEqual(test_module.beer, 1)

        add_9 = test_module.make_adder(9)
        self.assertEqual(add_9(0), 9)
        self.assertEqual(add_9(1), 10)


#
# The end.
