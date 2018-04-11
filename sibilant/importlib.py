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
sibilant.importlib

importlib extensions used for loading sibilant modules via the import
statement and opcodes.

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


import sys

from contextlib import contextmanager
from importlib.abc import FileLoader
from importlib.machinery import FileFinder, PathFinder

from os import getcwd

from .module import init_module, load_module
from .parse import source_str


# we're going to pre-import this
import sibilant.builtins  # noqa


# once installed, changing this does nothing.
_SOURCE_SUFFIXES = (".lspy", ".sibilant")


path_importer_cache = {}
path_hooks = []


__all__ = (
    "install", "is_installed",
    "SibilantPathFinder", "SibilantSourceFileLoader",
)


class SibilantPathFinder(PathFinder):
    """
    An overridden PathFinder which will hunt for sibilant files in
    sys.path. Uses storage in this module to avoid conflicts with the
    original PathFinder
    """


    @classmethod
    def invalidate_caches(cls):
        for finder in path_importer_cache.values():
            if hasattr(finder, 'invalidate_caches'):
                finder.invalidate_caches()


    @classmethod
    def _path_hooks(cls, path):
        for hook in path_hooks:
            try:
                return hook(path)
            except ImportError:
                continue
        else:
            return None


    @classmethod
    def _path_importer_cache(cls, path):
        if path == '':
            try:
                path = getcwd()
            except FileNotFoundError:
                # Don't cache the failure as the cwd can easily change to
                # a valid directory later on.
                return None
        try:
            finder = path_importer_cache[path]
        except KeyError:
            finder = cls._path_hooks(path)
            path_importer_cache[path] = finder
        return finder


class SibilantSourceFileLoader(FileLoader):


    def create_module(self, spec):
        # return None to let the Python system allocate its own
        # preferred module type
        return None


    def get_source(self, fullname):
        # with open(self.get_filename(fullname), "rt") as sf:
        #    data = sf.read()
        # return data

        return self.get_data(self.get_filename(fullname)).decode("utf8")


    def exec_module(self, module):
        name = module.__name__
        filename = self.get_filename(name)
        source_stream = source_str(self.get_source(name), filename=filename)

        init_module(module, source_stream)
        load_module(module)


class SibilantSourceZipFileLoader(SibilantSourceFileLoader):

    def __init__(self, fullname, path):
        pass


    def get_data(self, filename):
        pass


# go ahead and setup the path_hooks in advance. Even if we don't run
# install, this lets us run import_module


def _install():
    done = False

    def install():
        nonlocal done
        if done:
            return

        # teaches Python the meaning of our particular suffixes
        path_hooks.append(FileFinder.path_hook((SibilantSourceFileLoader,
                                                _SOURCE_SUFFIXES)))

        # teaches Python to actually look for sibilant's files
        sys.meta_path.append(SibilantPathFinder)
        sys.meta_path.append(SibilantZipPathFinder)

        done = True


    def is_installed():
        return done

    return install, is_installed


install, is_installed = _install()
del _install


#
# The end.
