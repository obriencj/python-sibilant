"""

author: siege@preoccupied.net

"""


from pyassem import PyFlowGraph



class CodeGenerator(object):

    
    def __init__(self, tree):
        self._cache = {}
        self.tree = tree
        self.graph = pyassem.PyFlowGraph("<module>", tree.filename)


    def default(self, node):
        for child in node.getChildNodes():
            self.visit(child)


    def visit(self, node):
        klass = node.__class__
        meth = self._cache.get(klass, None)
        
        if meth is None:
            className = klass.__name__
            meth = getattr(self, 'visit' + className, self.default)
            self._cache[klass] = meth

        return meth(node, *args)


    def walk(self):
        self.visit(self.tree)


    def get_code(self):
        return self.graph.getCode()


    def emit(self, bytecode, *args):
        self.graph.emit(bytecode, *args)



class NormalCodeGenerator(CodeGenerator):


    def visitOperator(self, node):
        pass


    def visitAnd(self, node):
        pass


    def visitBegin(self, node):
        pass


    def visitCall_CC(self, node):
        pass


    def visitComment(self, node):
        pass


    def visitCond(self, node):
        pass


    def visitEval(self, node):
        pass


    def visitIf(self, node):
        pass


    def visitLambda(self, node):
        pass


    def visitNot(self, node):
        pass


    def visitNumber(self, node):
        pass


    def visitOr(self, node):
        pass


    def visitPy_Function(self, node):
        pass


    def visitQuasiQuote(self, node):
        pass


    def visitQuote(self, node):
        pass


    def visitString(self, node):
        pass


    def visitSymbol(self, node):
        pass


    def visitUnquote(self, node):
        pass


    def visitUnquote_Splicing(self, node):
        pass



#
# The end.
