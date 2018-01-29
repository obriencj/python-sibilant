#! /usr/bin/env bash


VENV=".venv"

VPY="$VENV/bin/python"
VSIB="$VENV/bin/sibilant"


if test "$1" == "help" ; then
    cat <<EOF
Usage: $0 [COMMAND]

Builds and deploys into a virtualenv under $VENV

COMMAND may be one of 'sibilant' or 'python' in order to immediately
launch either the sibilant or python repl after installation.

EOF

    exit 1

else
    { virtualenv "$VENV" && "$VPY" setup.py clean build install ; } \
	|| exit $?

    if test "$1" == "sibilant" ; then
	$VSIB
    elif test "$1" == "python" ; then
	$VPY
    elif test -n "$1" ; then
	echo -e "\nUnknown command: $1"
	exit 1
    fi
fi


#
# The end.
