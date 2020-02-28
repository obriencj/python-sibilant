
FROM quay.io/obriencj/sibilant

LABEL maintainer = "obriencj@redhat.com" \
      version = "0.9.0"

# http://label-schema.org/rc1/
LABEL org.label-schema.schema-version = "1.0" \
      org.label-schema.name = "sibilant-flask-coolname" \
      org.label-schema.version = "0.9.0" \
      org.label-schema.url = "https://github.com/obriencj/sibilant" \
      org.label-schema.vcs-url = "https://github.com/obriencj/sibilant"


COPY sibfcn /sibfcn
RUN \
  pip install flask coolname


EXPOSE 80

ENV FLASK_APP sibfcn.app
ENV FLASK_RUN_PORT 80
ENV FLASK_RUN_HOST 0.0.0.0

ENTRYPOINT [ "python", "-m", "flask", "run" ]
CMD []

# The end.
