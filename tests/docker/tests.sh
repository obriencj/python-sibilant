#! /bin/bash

TESTNAME="$1"
LOGFILE=/src/logs/"$TESTNAME"-test.log

cd /src
mkdir -p logs

echo "Results for $TESTNAME" > "$LOGFILE"
python3 -u setup.py test flake8 >>"$LOGFILE" 2>&1


# The end.
