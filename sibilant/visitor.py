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
visitor utility class for Sibilant

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


class NoVisitMethod(Exception):
    pass


class Visitor(object):


    def visit(self, obj, *args, **kwds):
        """
        Finds a `visit<Type>` method and calls it with `node` and `*args`

        If no `visit<Type>` for the type of `node` is found, then the
        next type (by MRO) is checked, and so on. If there is no
        matching visit method for any type in the MRO of the node's
        class, the `default` method will be called.
        """

        # ensure we have the visit method cache
        if not hasattr(self, "_visit_k_cache"):
            self._visit_k_cache = {}
        cache = self._visit_k_cache

        method = None
        klass = type(obj)

        for k in klass.mro():
            method = cache.get(k, None)
            if not method:
                method = getattr(self, 'visit'+k.__name__, None)
                if method:
                    cache[klass] = method
                    cache[k] = method
            if method:
                break
        else:
            method = self.default
            cache[klass] = method

        return method(obj, *args, **kwds)


    def default(self, obj, *args, **kwds):
        """
        Raises a `NoVisitMethod`
        """

        raise NoVisitMethod(type(obj))


#
# The end.
