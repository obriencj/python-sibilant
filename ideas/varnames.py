#! /usr/bin/env python3

import ast


src1 = "lambda X: [X, locals()]"

ast1 = ast.parse(src1, mode="eval")
# ast1 is an <_ast.Expression object>
# ast1.body is an <_ast.Lambda object>

# change the name of the first argument to be something Python wouldn't
# normally allow
ast1.body.args.args[0].arg = "X-Value?"

# change the reference to the argument in the body of the lambda,
# which is in a literal list expression
ast1.body.body.elts[0].id = "X-Value?"

code1 = compile(ast1, "<input>", "eval")
fun1 = eval(code1)

print(fun1(1))
# >>> [1, {'X-Value?': 1}]


src2 = """
def is_list(obj):
  return isinstance(obj, list)
"""

ast2 = ast.parse(src2)
# ast2 is a <_ast.Module object>
# ast2.body[0] is a <_ast.FunctionDef object>"

# change the name of the function we're defining
ast2.body[0].name = "list?"

code2 = compile(ast2, "<input>", "exec")
glbls = {}
eval(code2, glbls)

print(glbls['list?'].__name__)
# >>> <function list?>
