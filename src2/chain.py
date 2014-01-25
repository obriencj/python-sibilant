"""
"""


import ast



def ChainCreator(ast.Visitor):


    def drop_cont(self, kchain):
        return [lambda: kchain, ]


    # returns an ATOMIC
    def chain_begin(self, nodes, kchain):
        
        if len(nodes) == 1:
            return self.visit(nodes[0], kchain)

        elif nodes:
            ncont = self.chain_begin(nodes[1:], kchain)
            
            # create an atomic taking only the continuation            
            self.pushblock(1)
            self.emittuple((ncont, ))
            return self.popblock()
        

    def chain_apply(self, nodes, kchain):
        pass


    def visitApply(self, node, kchain):
        pass


#
# The end.
