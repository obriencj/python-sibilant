# A tiny sibilant container based on Python 3.7 Alpine

FROM python:3.7-alpine as base


# An intermediary container which will have some of the necessary
# tools to build the binary extensions in gnureadline and
# sibilant. We'll deposit the results somewhere to be installed from
# later

FROM base as builder

COPY . /src

RUN \
  mkdir /wheels ; \
  apk add gcc make readline-dev musl-dev python3-dev ; \
  pip3 install --upgrade pip setuptools wheel ; \
  pip3 wheel -w /wheels gnureadline /src


# The real container, installing the wheels built into the /wheels dir
# from the intermediary one.

FROM base

COPY --from=builder /wheels /wheels
RUN \
  pip3 install /wheels/*.whl ; \
  rm -rf /wheels

CMD ["sibilant"]


# The end.
