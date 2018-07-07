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
sibilant.compiler.cpython36

author: Christopher O'Brien <obriencj@gmail.com>
license: LGPL v.3
"""


from sibilant.compiler import SibilantCompiler, gather_parameters
from sibilant.pseudops.targets.cpython36 import PseudopsCPython36
from sibilant.tco import tailcall, trampoline


class SibilantCPython36(PseudopsCPython36, SibilantCompiler):
    """
    Sibilant compiler for CPython 3.6
    """


    @trampoline
    def complete_apply(self, args, declared_at, tc, cont):
        params = gather_parameters(args)

        pos, keywords, values, vargs, vkwds = params

        assert (len(keywords) == len(values)), "mismatched keyword, values"

        if tc:
            self.helper_tailcall_tos(args, declared_at)

        arg_tuple = 0

        # step one, evaluate the positionals that we have
        for expr in pos:
            self.add_expression(expr)

        if not (vargs or keywords or vkwds):
            # easy mode, nothing fancy, just a plain 'ol call
            self.pseudop_call(len(pos))
            return tailcall(cont)(None, False)

        elif pos:
            # it's going to get complicated. collect the positionals
            # we've got into a tuple for later.
            self.pseudop_build_tuple(len(pos))
            arg_tuple += 1

        if vargs:
            # another tuple of positional arguments onto the pile
            self.add_expression(vargs)
            arg_tuple += 1

        if arg_tuple > 1:
            # if we have more than one positional tuple, join them
            # together into a single tuple (or if we have none, create
            # an empty tuple)
            self.pseudop_build_tuple_unpack(arg_tuple)

        if not (keywords or vkwds):
            # just positionals, so invoke CALL_FUNCTION_EX 0x00
            self.pseudop_call_var(0)
            return tailcall(cont)(None, False)

        elif not arg_tuple:
            # in order to support CALL_FUNCTION_EX later on, we're
            # going to push an empty tuple on, to represent our lack
            # of positionals
            self.pseudop_build_tuple(0)

        kwd_tuple = 0
        if keywords:
            # build a map out of all the keyword:value entries
            for key, val in zip(keywords, values):
                self.pseudop_const(str(key))
                self.add_expression(val)
            self.pseudop_build_map(len(keywords))
            kwd_tuple += 1

        if vkwds:
            # if we also have a variadic kwd, grab that.
            self.add_expression(vkwds)
            kwd_tuple += 1

        if kwd_tuple > 1:
            # if we have both keywords and variadic kwds, join' em together
            self.pseudop_build_map_unpack(kwd_tuple)

        if declared_at:
            self.pseudop_position(*declared_at)

        # even if we had no positionals, we've created an empty
        # positionals tuple, and now we can CALL_FUNCTION_EX 0x01
        self.pseudop_call_var_kw(0)

        return tailcall(cont)(None, False)


#
# The end.
