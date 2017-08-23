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
A contrived Sibilant sample project

author: Christopher O'Brien <obriencj@gmail.com>
license: LGPL v.3
"""


from setuptools import setup


TROVE_CLASSIFIERS = (
    "Development Status :: 4 - Beta",
    "Intended Audience :: Developers",
    "License :: OSI Approved"
    " :: GNU Lesser General Public License v3 or later (LGPLv3+)",
    "Operating System :: OS Independent",
    "Programming Language :: Python :: 3 :: Only",
    "Programming Language :: Python :: 3.5",
    "Programming Language :: Python :: 3.6",
    "Programming Language :: Python :: Implementation :: CPython",
    "Topic :: Software Development :: Libraries :: Python Modules",
)


setup(name = "sibilant-sample-project",
      version = "0.9.0",

      packages = ["sibsampl"],

      package_data = {
          "sibsampl": ["*.lspy"],
      },

      entry_points = {
          "console_scripts": [
              'sibsampl=sibsampl.cli:main',
          ],
      },

      requires = [
          "sibilant",
      ],

      # important -- currently zip_safe must be False in order for the
      # sibilant importer to discover .lspy files
      zip_safe = False,

      # PyPI information
      author = "Christopher O'Brien",
      author_email = "obriencj@gmail.com",
      url = "https://github.com/obriencj/python-sibilant/",
      license = "GNU Lesser General Public License v3",

      description = "A sibilant sample project",

      classifiers = TROVE_CLASSIFIERS)


#
# The end.
