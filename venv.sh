#! /usr/bin/env bash


VENV="$HOME/.virtualenvs"
BRANCH=$(git symbolic-ref --short -q HEAD)


VENV_BRANCH="$VENV/sibilant-$BRANCH"
VPY="$VENV_BRANCH/bin/python"
VSIB="$VENV_BRANCH/bin/sibilant"


if test "$1" == "help" ; then
    cat <<EOF
Usage: $0 [COMMAND]

Builds and deploys into a virtualenv under $VENV_BRANCH

COMMAND may be one of 'sibilant' or 'python' in order to immediately
launch either the sibilant or python repl after installation.

EOF

    exit 1

else
    echo -e "Current branch is $BRANCH so deploying into:\n $VENV_BRANCH"
    mkdir -p "$VENV_BRANCH"

    { virtualenv "$VENV_BRANCH" && "$VPY" setup.py clean build install ; } \
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
