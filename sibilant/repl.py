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

from traceback import format_exception, format_exception_only

from sibilant.module import init_module, load_module_1
from sibilant.parse import source_str


def repl(mod, stdin=sys.stdin, stdout=sys.stdout, stderr=sys.stderr):
    """
    enter into a read-eval-print-loop, using stdin, stdout, and stderr
    for user I/O. optionally include a sibilant module to act as a
    global namespace.

    returns the resulting global name space when the repl completes.
    """

    # print(file=stdout)
    env = mod.__dict__

    while True:
        try:
            line = input("sibilant > ")

            source = source_str(line)
            init_module(mod, source, None)

            result = load_module_1(mod)
            env['_'] = result
            if result is not None:
                print(result, file=stdout)

        except KeyboardInterrupt as ki:
            print(ki, file=stderr)
            stderr.flush()
            break

        except EOFError:
            print(file=stderr)
            stderr.flush()
            break

        except SyntaxError as rse:
            show_syntaxerr(file=stderr)
            stderr.flush()

        except Exception as se:
            show_traceback(file=stderr)
            stderr.flush()


        stdout.flush()

    print(file=stdout)
    return env


def show_syntaxerr(file=sys.stderr):
    type_, value, tb = sys.exc_info()
    sys.last_type = type_
    sys.last_value = value
    sys.last_traceback = tb

    lines = format_exception_only(type_, value)
    print(''.join(lines), file=file)


def show_traceback(skip=1, file=sys.stderr):
    sys.last_type, sys.last_value, last_tb = ei = sys.exc_info()
    sys.last_traceback = last_tb

    while skip >= 0:
        last_tb = last_tb.tb_next
        skip -= 1

    try:
        lines = format_exception(ei[0], ei[1], last_tb)
        print(''.join(lines), file=file)

    finally:
        last_tb = ei = None


#
# The end.
