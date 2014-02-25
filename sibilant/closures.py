"""
"""


from .dispatch import Dispatch


class ClosureInfo(Dispatch):


    def __init__(self):
        self.node_info = {}


    def nodeClosures(self, node):
        info = self.node_info[node]
        return info


    def dispatchSymbol(self, node):
        syms = set([node.name,])

        self.node_info[self] = syms
        return syms


    def dispatchApply(self, node):
        syms = set()

        syms.extend(self.dispatch(node.func))
        for p in node.args:
            syms.extend(self.dispatch(p))

        self.node_info[node] = syms
        return syms


    def dispatchLambda(self, node):
        syms = set()

        for b in node.body:
            syms.extend(self.dispatch(b))

        for formal in node.formals:
            syms.remove(formal.name)

        self.node_info[node] = syms
        return syms


    def dispatchLet(self, node):
        syms = set()

        for b in node.body:
            syms.extend(self.dispatch(b))

        for k,v in self.pairs:
            syms.remove(k.name)

        for k,v in self.pairs:
            syms.append(self.dispatch(v))

        self.node_info[node] = syms
        return syms


#
# The end.
