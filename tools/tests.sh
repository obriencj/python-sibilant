#! /bin/bash


function py3_impl_ver() {
    python3 -c 'import sys; print(sys.implementation.cache_tag);'
}


function run_tests() {
    local TESTNAME=$(py3_impl_ver)
    local LOGFILE=/src/logs/"$TESTNAME"-test.log

    mkdir -p /src/logs
    cd /src

    pip3 install --upgrade pip setuptools flake8 >/dev/null

    echo "Results for $TESTNAME" > "$LOGFILE"
    python3 -u setup.py clean build test flake8 >>"$LOGFILE" 2>&1
}


run_tests


# The end.
