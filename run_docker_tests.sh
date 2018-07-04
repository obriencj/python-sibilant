#! /bin/bash

# This script will use docker-compose to run the sibilant unit tests
# under python 3.5, 3.6, and 3.7

echo "Cleaning up binaries to force rebuild"
rm -rf ./build ./dist
find ./ -iname '*.so' -o -iname '*.dll' -o -iname '*.pyc' -exec rm -f {} \;

echo "Cleaning old test logs"
rm -rf ./logs/*test.log

echo "Running tests, output will be written to $PWD/logs/"
docker-compose -f ./docker/test-compose.yml up

# The end.
