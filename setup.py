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
Sibilant, Scheme for Python

author: Christopher O'Brien  <obriencj@gmail.com>
license: LGPL v.3
"""


from setuptools import setup


setup( name = "sibilant",
       version = "0.9.0",

       packages = [ "sibilant", ],

       test_suite = "tests",

       # PyPI information
       author = "Christopher O'Brien",
       author_email = "obriencj@gmail.com",
       url = "https://github.com/obriencj/python-sibilant",
       license = "GNU Lesser General Public License",

       description = "Scheme modules for Python"

       provides = [ "sibilant", ]
       requires = [],
       platforms = [ "python3 >= 3.3", ],

       zip_safe = False,

       classifiers = [ "Intended Audience :: Developers",
                       "Programming Language :: Python :: 3",
                       "Topic :: Software Development", ], )


#
# The end.
