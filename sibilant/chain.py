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
k call conformity chaining for sibilant

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


from dispatch import Dispatch


def collect_all_evaluate(k, items):
    """
    given a cons list of items, return a
    """

    args = list()
    for item in items[::-1]:
        args.append()

    yield (k, collected)


# (foo (bar 1) (baz 2) qux)
# (lambda a: (lambda a: (lambda a: (lambda a: (lambda a: (eval_k a)


def eval_(expr):
    result = list()
    eval_k(partial(result.append), expr)
    return result[0]


def eval_k(k, expr):
    while k is not None:
        k, expr = expr.__call_k__(k)


#
# The end.
