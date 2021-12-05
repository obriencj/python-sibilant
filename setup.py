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
Sibilant, a Lisp dialect for Python

:author: Christopher O'Brien  <obriencj@gmail.com>
:license: LGPL v.3
"""


def config():
    return {
        "packages": [
            "sibilant",
            "sibilant.compiler",
            "sibilant.compiler.targets",
            "sibilant.lib",
            "sibilant.pseudops",
            "sibilant.pseudops.targets",
            "sibilant.site",
        ],

        "package_data": {
            "sibilant": [
                "*.lspy",
            ],
            "sibilant.lib": [
                "*.h",
            ],
            "sibilant.site": [
                "*.lspy",
            ],
        },

        "headers": [
            "sibilant/lib/types.h",
        ],

        "ext_modules": [
            {
                "name": "sibilant.lib._types",
                "sources": [
                    "sibilant/lib/_types.c",
                    "sibilant/lib/atom.c",
                    "sibilant/lib/pair.c",
                    "sibilant/lib/tco.c",
                    "sibilant/lib/values.c",
                ],
                "extra_compile_args": [
                    "--std=c99",
                    "-g",
                    "-Wall",
                    "-Werror",
                ],
                "depends": [
                    "sibilant/lib/types.h",
                ],
                "include_dirs": [
                    "sibilant/lib",
                ],
            },
        ],

        "python_requires": ">=3.5, <3.8",

        "tests_require": [
            "asynctest",
        ],

        "zip_safe": False,

        "entry_points": {
            "console_scripts": [
                "sibilant=sibilant.cli:main",
            ],
        },
    }


def setup():
    from setuptools import setup, Extension

    data = config()
    exts = [Extension(**ext) for ext in data.pop("ext_modules", ())]
    data["ext_modules"] = exts

    setup(**data)


if __name__ == "__main__":
    setup()


#
# The end.
