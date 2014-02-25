"""
"""


from .visitor import Visitor


class CodeGenerator(Visitor):


    def __init__(self, node, name="<module>", filename="<string>"):
        self.tree = tree
        self.graph = pyassem.PyFlowGraph(name, filename)
        self.prep()


    def prep(self):
        pass


    def walk(self):
        return self.visit(self.tree)


    def get_code(self):
        return self.graph.getCode()


class TrampolineCodeGenerator(CodeGenerator):


    def prep(self):
        self.k = list()


    def createBeginCont(self, node):
        # pushes a block
        # accept parameter k
        # visit node
        # pop and returns block
        pass


    def visitBegin(self, node):
        # thread a continuation through all the members, such that
        # the first member is called with a continuation to call the second
        # member, with a continuation to call the third member, etc, until
        # the last member is called with our original continuation

        pass


    def visitPrint(self, node):
        # print TOS
        self.emit()


    def visitString(self, node):
        # add string to constants pool
        # push reference onto stack
        self.emit()


    def visitLambda(self, node):
        # presume k is TOS

        pass


    def visitApply(self, node):

        # ; for each param
        # if parameter is non-literal:
        #  load const x
        #

        # push k onto the stack
        # eval func, collect f into closure for arg1
        # eval arg1, collect f,a1 into closure for arg2
        # eval arg2, collect f,a1,a2 into closure for arg3
        # etc...
        # push a* onto stack
        # push f onto stack
        # call f

        pass


def evaluate(a, *b):
    while a:
        a, *b = a(*b)
    return b


#
# The end.
