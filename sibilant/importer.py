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
Importing this module will augment the python importing system to
be able to load sibilant modules.

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


import os, sys
from importlib.abc import FileLoader
from importlib.machinery import FileFinder, PathFinder
from sibilant.module import prep_module, exec_module


__all__ = ()


SOURCE_SUFFIXES = [".lspy", ".sibilant"]


_path_importer_cache = {}
_path_hooks = []


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
        if _path_hooks is not None and not _path_hooks:
            _warnings.warn('_path_hooks is empty', ImportWarning)

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
                path = os.getcwd()
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
        return self.get_data(self.get_filename(fullname)).decode("utf8")


    def exec_module(self, module):
        prep_module(module)
        exec_module(module, self.get_source(module.__name__))


def _get_lspy_file_loader():
    return (SibilantSourceFileLoader, SOURCE_SUFFIXES)


def _get_lspy_path_hook():
    return FileFinder.path_hook(_get_lspy_file_loader())


def __install():
    done = False
    def install():
        nonlocal done
        if not done:
            _path_hooks.append(_get_lspy_path_hook())
            sys.meta_path.append(SibilantPathFinder)
            done = False
    return install

__install = __install()
__install()


#
# The end.
