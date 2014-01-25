"""

author: siege@preoccupied.net

"""


def CompileMode(object):

    
    def __init__(self, source, filename):
        self.source = source
        self.filename = filename

        self.code = None
        self.tree = None


    def get_tree(self):
        from parse import parse_string

        if not self.tree:
            self.tree = parse_string(self.source)
        return self.tree


    def get_code_generator(self):
        return NormalGenerator()


    def get_code(self):
        if not self.code:
            tree = self.parse_source()
            gen = self.get_code_generator()
            self.code = gen.generate(tree)
            
        return self.code



def Expression(CompileMode):
    pass



def Interactive(CompileMode):
    
    def eval(self, glbls):
        return self.get_code().eval(glbls)



def Module(CompileMode):


    def dump(self, fd):
        import marshal
        
        fd.write(self.get_pyc_header())
        marshal.dump(self.get_code(), fd)


    def get_pyc_header(self):
        import imp, os.path

        mtime = os.path.getmtime(self.filename)
        mtime = struct.pack('<i', mtime)
        return imp.get_magic() + mtime



#
# The end.
