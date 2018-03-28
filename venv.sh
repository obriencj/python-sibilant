#! /usr/bin/env bash


VENV="$HOME/.virtualenvs"
BRANCH=$(git symbolic-ref --short -q HEAD 2>/dev/null || \
	 echo "unknown-branch")

VDIR="$VENV/sibilant-$BRANCH"
VBIN="$VDIR/bin"

CMD="$1"
shift


SYSPYTHON=$(which python3)
if test ! -x "$SYSPYTHON" ; then
    echo "Error: Could not find python3, exiting."
    exit 1
fi

# prevent virtualenv from ending up with too deeply nested symlinks
SYSPYTHON=$(readlink -f "$SYSPYTHON")


if test ! -x "$(which virtualenv)" ; then
    echo "Error: Could not find virtualenv, exiting."
    exit 1
fi


if test "$CMD" == "help" || test -z "$CMD" ; then
    cat <<EOF
Usage: $0 COMMAND [CMD_OPTS]

Builds and deploys into a virtualenv under:
 $VDIR

COMMAND may be one of the following:

  help          show this message and exit
  init          setup (and clear if existing) a basic virtualenv
  setup         run setup.py from the virtualenv (hint: setup install)
  python        run python from the virtualenv
  pip           run pip from the virtualenv
  pdb-python    run python in the virtualenv from pdb
  pdb-sibilant  run sibilant in the virtualenv from pdb
  sibilant      run sibilant from the virtualenv
  sys-python    run system python with PYTHONHOME set (fixes wx issues)
  sys-sibilant  run sibilant using system python with PYTHONHOME set

CMD_OPTS are passed to the handler for the COMMAND (where applicable)

EOF
    exit 1
fi


echo -e "Current branch is $BRANCH so working in:\n $VDIR"

case "$CMD" in
    init)
	mkdir -p "$VDIR"
	virtualenv --python="$SYSPYTHON" "$VDIR" "$@" || exit $?
	;;

    install)
	virtualenv --python="$SYSPYTHON" "$VDIR" || exit $?
	"$VBIN/python" setup.py install || exit $?
	;;

    setup|setup.py)
	"$VBIN/python" setup.py "$@" || exit $?
	;;

    sibilant)
	"$VBIN/sibilant" "$@"
	;;

    python)
	"$VBIN/python" "$@"
	;;

    pip)
	"$VBIN/pip" "$@"
	;;

    pdb-python)
	"$VBIN/python" -m pdb "$@"
	;;

    pdb-sibilant)
	"$VBIN/python" -m pdb "$@" "$VBIN/sibilant"
	;;

    sys-python)
	# wxPython doesn't like virtualenv very much on macOS. You
	# have to run the system version, and get it to operate inside
	# the venv by setting the PYTHONHOME parameter.
	PYTHONHOME="$VDIR" "$SYSPYTHON" "$@"
	;;

    sys-sibilant)
	PYTHONHOME="$VDIR" "$SYSPYTHON" "$VBIN/sibilant" "$@"
	;;

    *)
	echo -e "\nUnknown command: $CMD"
	exit 1
	;;
esac


#
# The end.
