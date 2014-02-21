


class Printer(ast.Visitor):

    def __init__(self, outstream):
        self.out = outstream
        self.line = 0


    def linejump(self, toline):
        delta = toline - self.line
        if delta > 0:
            self.write("\n" * delta)
            self.line = toline


    def write(self, s):
        self.out.write(s)


    def advance(self):
        self.write(" ")


    def emitOpen(self):
        self.write("(")


    def emitClose(self):
        self.write(")")


    def emitToken(self, k):
        self.write(k)


    def visit(self, node):
        ast.Visitor.visit(self, node)
        self.advance()


    def visitApply(self, node):
        self.emitOpen()
        self.visit(node.func)
                   

    def visitAtom(self, node):
        self.write(node.token)


    def visitLambda(self, node):
        self.emitOpen()
        self.write("lambda")
        self.advance()
        
        self.emitOpen()
        for f in node.formals:
            self.visit(f)
        self.emitClose()


#
# The end.
