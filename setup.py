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


def setup_data():
    # In order to make the build process more data-driven, I've opted
    # to store all of the build configuration in the setup.json
    # file. This allows me to re-use the data in the Dockerfile and
    # potentially elsewhere

    from json import load

    with open("setup.json") as dat:
        return load(dat)


def setup():
    from setuptools import setup, Extension

    data = setup_data()
    exts = [Extension(**ext) for ext in data.pop("ext_modules", ())]
    data["ext_modules"] = exts

    setup(**data)


if __name__ == "__main__":
    setup()


#
# The end.
