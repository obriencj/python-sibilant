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

:author: Christopher O'Brien  <obriencj@gmail.com>
:license: LGPL v.3
"""


from sys import exit, version_info
from setuptools import setup


if (3, 6) <= version_info <= (3, 7):
    pass
elif (3, 5) <= version_info <= (3, 6):
    pass
else:
    exit("unsupported version: %r" % version_info)


setup(name = "sibilant",
      version = "0.9.0",

      packages = [
          "sibilant",
          "sibilant.compiler",
      ],

      package_data = {
          "sibilant": ["*.lspy"],
      },

      test_suite = "tests",

      entry_points = {
          "console_scripts": [
              'sibilant=sibilant.cli:main',
          ],
      },

      # PyPI information
      author = "Christopher O'Brien",
      author_email = "obriencj@gmail.com",
      url = "https://github.com/obriencj/python-sibilant",
      license = "GNU Lesser General Public License",

      description = "LISP dialect for Python",

      provides = ["sibilant", ],
      requires = ["appdirs", ],
      platforms = ["python3 >= 3.5", ],

      zip_safe = True,

      classifiers = [
          "Intended Audience :: Developers",
          "Programming Language :: Python :: 3 :: Only",
          "Programming Language :: Python :: 3.5",
          "Programming Language :: Python :: 3.6",
          "Programming Language :: Python :: Implementation :: CPython",
          "Topic :: Software Development", ],
)


#
# The end.
