FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

ARG DR_TAG
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

RUN apt-get update \
  && apt-get -y --no-install-recommends install rsyslog rsyslog-relp logrotate \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

COPY rsyslog.conf /etc/rsyslog.conf
COPY logrotate.conf /etc/logrotate.conf
COPY entrypoint.d/*.sh /usr/local/entrypoint.d/
