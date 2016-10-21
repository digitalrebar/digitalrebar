FROM gcr.io/google_containers/ubuntu-slim:0.5

ENV SQUID_CACHE_DIR=/var/spool/squid3 \
    SQUID_LOG_DIR=/var/log/squid3 \
    SQUID_USER=proxy

COPY install.sh /tmp
COPY rules.path /tmp

RUN mkdir -p /debs && /tmp/install.sh

COPY /tmp/*.deb /debs
