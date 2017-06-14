# This library is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 3 of the
# License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, see
# <http://www.gnu.org/licenses/>.


"""
Read-Eval-Print-Loop for sibilant

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


def repl(stdin, stdout, stderr, glbls=None):
    """
    enter into a read-eval-print-loop, using stdin, stdout, and stderr
    for user I/O. optionally include a sibilant module to act as a
    global namespace.

    returns the resulting global name space when the repl completes.
    """

    glbls = glbls or module("__repl__")

    while True:
        try:
            ast = read_ast(stdin, prompt="(repl) >")
            expr = compile_ast(ast, glbls)
            result = evaluate_expr(expr, glbls)
            glbls['_'] = result
            print_result(stdout, result)

        except SibilantError as se:
            print_error(stderr, se)

        except KeyboardInterrupt:
            break

        except StopRepl:
            break

    return glbls


#
# The end.
