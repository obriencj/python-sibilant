"""
"""


from .dispatch import Dispatch

import compiler.ast as pyast
import ast


class PythonicTranslator(Dispatch):


    def collectChain(self, node, k):
        pass


    def dropChain(self, nodelist, k):
        now, next = nodelist[0], nodelist[1:]

        expr = pyast.Lambda()
        #expr.add(pyast.Tuple(self.dispatch(now), self.dropChain(

        if isatom(now):
            expr.add(pyast.Tuple(self.dropChain(next, k), self.dispatch(now)))
        else:
            pass
            #expr.add(pyast.Tuple(self.

        return expr


    def dispatchApply(self, node, k):
        pass


#
# The end.
