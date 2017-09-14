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
Sibilant Example Project

:author: Christopher O'Brien  <obriencj@gmail.com>
:license: LGPL v.3
"""


# this is the only part that is really important. You need to have
# your top-level package or module enable the sibilant importer. The
# easiest way to do that is to just import sibilant.importer

import sibilant.importer  # noqa


if __name__ == "__main__":
    import sys, siblife.cli
    sys.exit(siblife.cli.main())


#
# The end.
