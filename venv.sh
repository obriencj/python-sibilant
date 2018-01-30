#! /usr/bin/env bash


VENV="$HOME/.virtualenvs"
BRANCH=$(git symbolic-ref --short -q HEAD)

VDIR="$VENV/sibilant-$BRANCH"
VBIN="$VDIR/bin"

CMD="$1"
shift


SYSPYTHON=$(readlink -f $(which python3))


if test "$CMD" == "help" || test -z "$CMD" ; then
    cat <<EOF
Usage: $0 COMMAND

Builds and deploys into a virtualenv under:
 $VDIR

COMMAND may be one of the following:

  help          show this message and exit
  install       create the virtualenv and install into it
  sibilant      run sibilant from the virtualenv
  python        run python from the virtualenv
  pip           run pip from the virtualenv
  sys-python    run system python with PYTHONHOME set (fix wx issues)
  sys-python    run sibilant using system python with PYTHONHOME set

EOF
    exit 1
fi


echo -e "Current branch is $BRANCH so working in:\n $VDIR"

case "$CMD" in
    install)
	mkdir -p "$VDIR"

	virtualenv --python="$SYSPYTHON" "$@" "$VDIR" || exit $?
	"$VBIN/python" setup.py clean build install || exit $?

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
