"""

Sibilant test cases

author: siege@preoccupied.net

"""


import unittest



class TestExpressions(unittest.TestCase):


    def assertEvalsEqual(self, src, *values):
        import sibilant
        res = sibilant.eval_from_str(src, globals())
        self.assertEqual(res[0], values)
        

    def testToken(self):
        src = """8"""
        self.assertEvalsEqual(src, 8)


    def testBegin(self):
        src = """(begin 100 200 300 400)"""
        self.assertEvalsEqual(src, 400)


    def testAdd(self):
        src = """(+ 100 5)"""
        self.assertEvalsEqual(src, 105)


    def testSubtract(self):
        src = """(- 100 5)"""
        self.assertEvalsEqual(src, 95)


    def testAnd_1(self):
        src = """(and False True)"""
        self.assertEvalsEqual(src, False)


    def testAnd_2(self):
        src = """(and True False)"""
        self.assertEvalsEqual(src, False)


    def testAnd_3(self):
        src = """(and True True)"""
        self.assertEvalsEqual(src, True)


    def testOr_1(self):
        src = """(or False True)"""
        self.assertEvalsEqual(src, True)


    def testOr_2(self):
        src = """(or True False)"""
        self.assertEvalsEqual(src, True)


    def testOr_3(self):
        src = """(or False False)"""
        self.assertEvalsEqual(src, False)


    def testIf_1(self):
        src = """(if True 70 80)"""
        self.assertEvalsEqual(src, 70)


    def testIf_2(self):
        src = """(if False 70 80)"""
        self.assertEvalsEqual(src, 80)


    def testLambda_1(self):
        src = """((lambda (x y) (+ x y)) 40 9)"""
        self.assertEvalsEqual(src, 49)


    def testLambda_2(self):
        src = """(((lambda (x) (lambda (y) (+ x y))) 40) 9)"""
        self.assertEvalsEqual(src, 49)


    def testLet(self):
        src = """(let ((a 70) (b 8)) (+ a b))"""
        self.assertEvalsEqual(src, 78)


    #def testNamedLet(self):
    #    src =  ("""
    #    (let wicked ((a 70) (b 1))
    #      (+ a (if (> b 0) (wicked 10 0) 5)))
    #    """)
    #
    #    self.assertEvalsEqual(src, 85)



def suite():
    loader = unittest.TestLoader()

    tests = [
        loader.loadTestsFromTestCase(TestExpressions) ]

    return unittest.TestSuite(tests)



def main():
    runner = unittest.TextTestRunner()
    runner.run(suite())



if __name__ == "__main__":
    main()



#
# The end.
