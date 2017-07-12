"""
"""


from .dispatch import Dispatch


class CodeGenerator(Dispatch):


    def __init__(self, node, name="<module>", filename="<string>"):
        self.tree = list()
        self.graph = pyassem.PyFlowGraph(name, filename)


    def walk(self):
        return self.dispatch(self.tree)


    def get_code(self):
        return self.graph.getCode()


    def dispatchBegin(self, node):
        pass


    def dispatchPrint(self, node):
        pass


    def dispatchString(self, node):
        pass


    def dispatchLambda(self, node):
        pass


    def dispatchApply(self, node):
        pass




#
# The end.
