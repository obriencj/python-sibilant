# A tiny sibilant container based on Python 3.7 Alpine

FROM python:3.7-alpine as base


# An intermediary container which will have some of the necessary
# tools to build the binary extensions in sibilant. We'll deposit the
# resulting wheel to be installed later

FROM base as builder

COPY setup.py /build/setup.py
COPY sibilant /build/sibilant/

RUN \
  apk add gcc musl-dev python3-dev && \
  mkdir /wheels && \
  pip wheel -w /wheels /build/


# The real container, installing the wheels built into the /wheels dir
# from the intermediary one.

FROM base

COPY --from=builder /wheels /wheels
RUN \
  pip install /wheels/*.whl && \
  rm -rf /wheels


CMD sibilant


# The end.
