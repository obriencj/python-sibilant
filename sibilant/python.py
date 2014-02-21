"""
"""


import visitor
import compiler.ast as pyast
import ast




class PythonicTranslator(visitor.Visitor):


    def collectChain(self, node, k):
        pass


    def dropChain(self, nodelist, k):
        now, next = nodelist[0], nodelist[1:]
        
        expr = pyast.Lambda()
        expr.add(pyast.Tuple(self.visit(now), self.dropChain(

        if isatom(now):
            expr.add(pyast.Tuple(self.dropChain(next, k), self.visit(now)))
        else:
            expr.add(pyast.Tuple(self.
            
        return expr


    def visitApply(self, node, k):
        


    



#
# The end.
