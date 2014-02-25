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
mro dispatch utility class for Sibilant

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


class NoDispatchMethod(Exception):
    pass


class Dispatch(object):

    _dispatch_prefix = "dispatch"


    def dispatch(self, obj, *args, **kwds):
        """
        Finds a `dispatch<Type>` method and calls it with `node` and
        `*args`

        If no `dispatch<Type>` for the type of `node` is found, then
        the next type (by MRO) is checked, and so on. If there is no
        matching `dispatch...` method for any type in the MRO of the
        node's class, the `default` method will be called.
        """

        # ensure we have the visit method cache
        if not hasattr(self, "_dispatch_k_cache"):
            self._visit_k_cache = {}
        cache = self._visit_k_cache

        method = None
        klass = type(obj)

        for k in klass.mro():
            method = cache.get(k, None)
            if not method:
                nom = self._dispatch_prefix + k.__name__
                method = getattr(self, nom, None)
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
        Raises a `NoDispatchMethod`
        """

        raise NoDispatchMethod(type(obj))


#
# The end.
