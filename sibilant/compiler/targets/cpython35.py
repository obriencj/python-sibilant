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
sibilant.compiler.cpython35

author: Christopher O'Brien <obriencj@gmail.com>
license: LGPL v.3
"""


from sibilant.compiler import SibilantCompiler, gather_parameters
from sibilant.pseudops.targets.cpython35 import PseudopsCPython35
from sibilant.lib import tailcall, trampoline


class SibilantCPython35(PseudopsCPython35, SibilantCompiler):
    """
    Sibilant compiler for CPython 3.5
    """


    @trampoline
    def complete_apply(self, args, declared_at, tc, cont):

        params = gather_parameters(args)

        pos, keywords, values, vargs, vkwds = params

        assert (len(keywords) == len(values)), "mismatched keyword, values"

        if tc:
            self.helper_tailcall_tos(args, declared_at)

        for expr in pos:
            self.add_expression(expr)

        if vargs:
            self.add_expression(vargs)

        for key, val in zip(keywords, values):
            self.pseudop_const(str(key))
            self.add_expression(val)

        if vkwds:
            self.add_expression(vkwds)

        if vargs:
            if vkwds:
                # CALL_FUNCTION_VAR_KW
                pseu = self.pseudop_call_var_kw
            else:
                # CALL_FUNCTION_VAR
                pseu = self.pseudop_call_var
        else:
            if vkwds:
                # CALL_FUNCTION_KW
                pseu = self.pseudop_call_kw
            else:
                # CALL_FUNCTION
                pseu = self.pseudop_call

        if declared_at:
            self.pseudop_position(*declared_at)

        pseu(len(pos), len(keywords))
        return tailcall(cont)(None, False)


#
# The end.
