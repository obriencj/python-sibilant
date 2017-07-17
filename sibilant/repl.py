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


import sys
import sibilant.builtins

from sibilant.ast import compose_all_from_str
from sibilant.compiler import compile_from_ast


def basic_env(**base):
    env = {"__builtins__": sibilant.builtins}
    env.update(base)
    return env


def repl(stdin=sys.stdin, stdout=sys.stdout, stderr=sys.stderr, **glbls):
    """
    enter into a read-eval-print-loop, using stdin, stdout, and stderr
    for user I/O. optionally include a sibilant module to act as a
    global namespace.

    returns the resulting global name space when the repl completes.
    """

    env = basic_env(stdin=stdin, stdout=stdout, stderr=stderr, **glbls)

    # print(file=stdout)
    print("sibilant > ", end="", file=stdout)
    stdout.flush()

    for line in stdin:
        try:
            for astree in compose_all_from_str(line):
                code = compile_from_ast(astree, env)
                result = eval(code, env)

                env['_'] = result
                if result is None:
                    print(file=stdout)
                else:
                    print(result, file=stdout)

        except KeyboardInterrupt as ki:
            print(se, file=stderr)
            stderr.flush()
            break

        except Exception as se:
            print(se, file=stderr)
            stderr.flush()

        print("sibilant > ", end="", file=stdout)
        stdout.flush()

    print(file=stdout)
    return env


#
# The end.
