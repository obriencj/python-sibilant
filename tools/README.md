
# Docker Composes

## debug-compose.yml

This will load the cpython3?.debug images. It doesn't run anything in
them, it simply allows them to be built easily. Each of these images
will contain their version of Python compiled with debugging enabled,
so that the lldebug flag can be used.


## test-compose.yml

This will load CPython images for 3.5, 3.6, and 3.7 to execute
tests.sh in each. The results of which will be collected in the logs
directory.


# Docker Images

## cpython3?.debug

This is an image with Python installed with debugging enabled.


# Supporting Files

## gdbinit

This is copied into the cpython3?.debug images when they are created,
and acts as the baseline gdb configuration.


## tests.sh

This is intended to be run inside of the cpython3?.test containers
when invoked using the test-compose.yml
