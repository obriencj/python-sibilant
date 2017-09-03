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
importlib extensions used for loading sibilant modules.

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


import sys

from contextlib import contextmanager
from importlib.abc import FileLoader
from importlib.machinery import FileFinder, PathFinder

from os import getcwd
from os.path import basename, getmtime, getsize

from sibilant.module import (
    prep_module, exec_module, init_module, marshal_module,
)


# we're going to pre-import this
import sibilant.builtins  # noqa


# once installed, changing this does nothing.
_SOURCE_SUFFIXES = (".lspy", ".sibilant")


_path_importer_cache = {}
_path_hooks = []


__all__ = (
    "install", "is_installed", "import_module",
)


class SibilantPathFinder(PathFinder):
    """
    An overridden PathFinder which will hunt for sibilant files in
    sys.path. Uses storage in this module to avoid conflicts with the
    original PathFinder
    """


    @classmethod
    def invalidate_caches(cls):
        for finder in _path_importer_cache.values():
            if hasattr(finder, 'invalidate_caches'):
                finder.invalidate_caches()


    @classmethod
    def _path_hooks(cls, path):
        for hook in _path_hooks:
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
            finder = _path_importer_cache[path]
        except KeyError:
            finder = cls._path_hooks(path)
            _path_importer_cache[path] = finder
        return finder


class SibilantSourceFileLoader(FileLoader):


    def create_module(self, spec):
        return None


    def get_source(self, fullname):
        # return self.get_data(self.get_filename(fullname)).decode("utf8")

        with open(self.get_filename(fullname), "rt") as sf:
            data = sf.read()
        return data


    def exec_module(self, module):
        name = module.__name__
        source = self.get_source(name)
        filename = basename(self.get_filename(name))

        prep_module(module)
        exec_module(module, source, filename=filename)


# go ahead and setup the _path_hooks in advance. Even if we don't run
# install, this lets us run import_module
_path_hooks.append(FileFinder.path_hook((SibilantSourceFileLoader,
                                         _SOURCE_SUFFIXES)))


def _install():
    done = False

    def install():
        nonlocal done
        if not done:
            sys.meta_path.append(SibilantPathFinder)
            done = True

    def is_installed():
        return done

    return install, is_installed


install, is_installed = _install()


@contextmanager
def temporary_install():
    if is_installed():
        yield None
    else:
        sys.meta_path.append(SibilantPathFinder)
        yield None
        sys.meta_path.remove(SibilantPathFinder)


def import_module(name, globals_=None, locals_=None, fromlist=0, level=0):
    with temporary_install():
        return __import__(name, globals_, locals_, fromlist, level)


def compile_to_file(name, source_file, dest_file):
    mtime = getmtime(source_file)
    source_size = getsize(source_file)

    mod = init_module(name, builtins=sibilant.builtins, filename=source_file)

    with open(source_file, "rt") as fin:
        bytecode = marshal_module(mod, fin, filename=source_file,
                                  mtime=mtime, source_size=source_size)

    with open(dest_file, "wb") as fout:
        fout.write(bytecode)


#
# The end.
