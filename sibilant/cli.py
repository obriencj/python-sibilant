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
Sibilant, a Scheme for Python

:author: Christopher O'Brien  <obriencj@gmail.com>
:license: LGPL v.3
"""


import sys

from appdirs import AppDirs
from argparse import ArgumentParser
from os.path import basename, join

from .repl import repl
from .module import create_module


_APPDIR = AppDirs("sibilant")

DEFAULT_HISTFILE = join(_APPDIR.user_config_dir, "history")


def cli(options):
    """
    Run as from the command line, with the given options argument and
    additional positional args
    """

    if options.importer:
        # this has the side-effect of augmenting the import system to
        # search for sibilant source files in the sys path. If
        # --no-importer was specified, then don't do that!
        import sibilant.importer  # noqa

    if options.tweakpath:
        sys.path.insert(0, ".")

    filename = options.filename

    if filename:
        with open(filename, "rt") as fd:
            mod = create_module("__main__", fd, filename=filename)

        if options.interactive:
            # probably not the best way to implement this, but it'll
            # do for now, eh?
            repl(**mod.__dict__)

    else:
        repl(__name__="__main__", __file__=None)


def cli_option_parser(args):
    """
    Create an `ArgumentParser` instance with the options requested by
    the `cli` function
    """

    parser = ArgumentParser(prog=basename(args[0]))

    parser.add_argument("filename", nargs="?", default=None)

    parser.add_argument("--no-importer", dest="importer",
                        action="store_false", default=True,
                        help="Do not enable the sibilany importer extension")

    parser.add_argument("--no-tweak-path", dest="tweakpath",
                        action="store_false", default=True,
                        help="Do not add the current directory to sys.path")

    parser.add_argument("--histfile", dest="histfile",
                        action="store", default=DEFAULT_HISTFILE,
                        help="REPL history file")

    parser.add_argument("-i", "--interactive", dest="interactive",
                        action="store_true", default=False,
                        help="Enter interactive mode after executing the"
                        " given script")

    return parser


def main(args=sys.argv):
    """
    Entry point for the REPL
    """

    parser = cli_option_parser(args)
    options = parser.parse_args(args[1:])

    # todo: arg checking, emit problems using `parser.error`

    try:
        cli(options)

    except KeyboardInterrupt:
        print(file=sys.stderr)
        return 130

    else:
        return 0


if __name__ == "__main__":
    sys.exit(main())


#
# The end.
