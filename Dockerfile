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

LABEL maintainer = "obriencj@redhat.com" \
      version = "0.9.0"

# http://label-schema.org/rc1/
LABEL org.label-schema.schema-version = "1.0" \
      org.label-schema.name = "sibilant" \
      org.label-schema.version = "0.9.0" \
      org.label-schema.url = "https://github.com/obriencj/sibilant" \
      org.label-schema.vcs-url = "https://github.com/obriencj/sibilant"


COPY --from=builder /wheels /wheels
RUN \
  pip install /wheels/*.whl && \
  rm -rf /wheels


CMD sibilant


# The end.
