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

from traceback import print_exc

from sibilant.ast import compose_all_from_str
from sibilant.compiler import compile_from_ast


def basic_env(**base):
    base["__builtins__"] = sibilant.builtins
    return base


def repl(stdin=sys.stdin, stdout=sys.stdout, stderr=sys.stderr,
         env=None):
    """
    enter into a read-eval-print-loop, using stdin, stdout, and stderr
    for user I/O. optionally include a sibilant module to act as a
    global namespace.

    returns the resulting global name space when the repl completes.
    """

    if env is None:
        env = basic_env(stdin=stdin, stdout=stdout, stderr=stderr)

    # print(file=stdout)
    print("sibilant > ", end="", file=stdout)
    stdout.flush()

    for line in stdin:
        try:
            for astree in compose_all_from_str(line):
                code = compile_from_ast(astree, env)
                result = eval(code, env)

                env['_'] = result
                if result is not None:
                    print(result, file=stdout)

        except KeyboardInterrupt as ki:
            print(ki, file=stderr)
            stderr.flush()
            break

        except Exception as se:
            print_exc(file=stderr)
            stderr.flush()

        print("sibilant > ", end="", file=stdout)
        stdout.flush()

    print(file=stdout)
    return env


#
# The end.
