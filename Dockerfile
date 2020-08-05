FROM alpine:latest

RUN apk add --no-cache emacs-nox

COPY bin/eldev /bin/eldev

# Initiate bootstrap
RUN eldev version

CMD ["eldev", "version"]
