#! /usr/bin/env python3


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
Sibilant, a LISP for Python

:author: Christopher O'Brien  <obriencj@gmail.com>
:license: LGPL v.3
"""


from os.path import join
from setuptools import setup, Extension


_extensions = []
_headers = ["ext/sibilant.h"]


def declare_ext(name, filename, header=None):

    e = Extension(name,
                  sources=[join("ext", filename)],
                  extra_compile_args=["--std=c99"])
    _extensions.append(e)

    if header:
        _headers.append(join("ext", header))


declare_ext("sibilant._atom", "atom.c", "atom.h")
declare_ext("sibilant._pair", "pair.c", "pair.h")
declare_ext("sibilant._tco", "tco.c", "tco.h")
declare_ext("sibilant._util", "util.c")
declare_Ext("sibilant._values", "values.c", "values.h")


setup(
    name = "sibilant",
    version = "0.9.0",

    packages = [
        "sibilant",
        "sibilant.compiler",
    ],

    package_data = {
        "sibilant": ["*.lspy"],
    },

    ext_modules = _extensions,
    headers = _headers,

    test_suite = "tests",

    entry_points = {
        "console_scripts": [
            'sibilant=sibilant.cli:main',
        ],
    },

    # todo: make this optional and just for cli
    install_requires = ["appdirs", ],

    # targets only support Python 3.5 through 3.6
    python_requires = ">=3.5, <3.7",

    description = "LISP dialect for Python",

    # PyPI information
    author = "Christopher O'Brien",
    author_email = "obriencj@gmail.com",
    url = "https://github.com/obriencj/python-sibilant",
    license = "GNU Lesser General Public License",

    zip_safe = True,

    classifiers = [
        "Intended Audience :: Developers",
        "Programming Language :: Python :: 3 :: Only",
        "Programming Language :: Python :: 3.5",
        "Programming Language :: Python :: 3.6",
        "Programming Language :: Python :: Implementation :: CPython",
        "Topic :: Software Development",
    ],
)


#
# The end.
