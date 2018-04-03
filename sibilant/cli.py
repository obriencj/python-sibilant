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
sibilant.cli

Command-line interface for invoking the sibilant repl or for compiling
sibilant source files into pyc files

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


import sys

from appdirs import AppDirs
from argparse import ArgumentParser
from os.path import basename, join

from .module import new_module, init_module, load_module, compile_to_file
from .parse import source_open
from .repl import repl


_APPDIR = AppDirs("sibilant")

DEFAULT_HISTFILE = join(_APPDIR.user_config_dir, "history")


class CLIException(Exception):
    pass


def cli_compile(options):

    filename = options.filename
    if not filename:
        raise CLIException("--compile requires that FILENAME is specified")

    name = options.compile

    if filename.endswith(".lspy"):
        destname = filename[:-4] + "pyc"
    elif filename.endswith(".sibilant"):
        destname = filename[:-8] + "pyc"
    else:
        raise CLIException("FILENAME should be a .lspy or .sibilant file")

    compile_to_file(name, filename, destname)


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

    if options.compile:
        return cli_compile(options)

    mod = new_module("__main__")

    filename = options.filename
    if filename:
        with source_open(filename) as source:
            init_module(mod, source, filename=filename)
            load_module(mod)

        if not options.interactive:
            return

    repl(mod)


def cli_option_parser(name):
    """
    Create an `ArgumentParser` instance with the options requested by
    the `cli` function
    """

    parser = ArgumentParser(prog=basename(name))

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

    g = parser.add_mutually_exclusive_group()

    g.add_argument("-i", "--interactive", dest="interactive",
                   action="store_true", default=False,
                   help="Enter interactive mode after executing the"
                   " given script")

    g.add_argument("-C", "--compile", dest="compile",
                   action="store", default=None,
                   help="Compile the specified file as a module with"
                   " this name")

    return parser


def main(args=sys.argv):
    """
    Entry point for the REPL
    """

    # we HAVE to tweak args to make it appear that sibilant is $0
    # there are so many libraries that break if we don't.

    name, *args = args
    sys.argv = list(args)

    parser = cli_option_parser(name)
    options = parser.parse_args(args)

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
