"""
"""


import visitor



class ClosureInfo(visitor.Visitor):


    def __init__(self):
        self.node_info = {}


    def nodeClosures(self, node):
        info = self.node_info[node]
        return info


    def default(self, node):
        pass


    def visitSymbol(self, node):
        syms = set([node.name,])
        
        self.node_info[self] = syms
        return syms
    

    def visitApply(self, node):
        syms = set()
        
        syms.extend(self.visit(node.func))
        for p in node.args:
            syms.extend(self.visit(p))

        self.node_info[node] = syms
        return syms
    

    def visitLambda(self, node):
        syms = set()
        
        for b in node.body:
            syms.extend(self.visit(b))

        for formal in node.formals:
            syms.remove(formal.name)

        self.node_info[node] = syms
        return syms


    def visitLet(self, node):
        syms = set()

        for b in node.body:
            syms.extend(self.visit(b))

        for k,v in self.pairs:
            syms.remove(k.name)

        for k,v in self.pairs:
            syms.append(self.visit(v))

        self.node_info[node] = syms
        return syms



#
# The end.
