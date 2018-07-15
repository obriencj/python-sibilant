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

from sibilant.lib import car, cdr, cons, nil, symbol
from sibilant.module import new_module, init_module, load_module
from sibilant.parse import source_str


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


mod_source_1 = """
(define tacos 5)
(define beer 3)

(define make_adder
  (lambda (by) (lambda (y) (+ y by))))

(let ((add-8 (make_adder (+ beer tacos))))
  (set_result (add-8 100)))

;; (raise tacos)
"""


class ModuleTest(TestCase):

    def test_1(self):
        # we'll pass setter in to the pre-defined globals
        getter, setter = getter_setter(None)

        defaults = {"set_result": setter}

        source = source_str(mod_source_1, "<unittest>")
        test_module = new_module("test_module")

        init_module(test_module, source, defaults=defaults)
        load_module(test_module)

        # the last action of the module is to call set_result, and
        # getter can show us what was passed there.
        self.assertEqual(getter(), 108)

        self.assertEqual(test_module.tacos, 5)
        self.assertEqual(test_module.beer, 3)

        add_9 = test_module.make_adder(9)
        self.assertEqual(add_9(0), 9)
        self.assertEqual(add_9(1), 10)


#
# The end.
