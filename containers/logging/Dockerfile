FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

RUN apt-get update && apt-get -y install rsyslog rsyslog-relp logrotate

COPY rsyslog.conf /etc/rsyslog.conf
COPY logrotate.conf /etc/logrotate.conf
COPY external-logging.json /root/external-logging.json
COPY internal-logging.json /root/internal-logging.json
COPY external-logging-relp.json /root/external-logging-relp.json
COPY internal-logging-relp.json /root/internal-logging-relp.json
COPY entrypoint.d/*.sh /usr/local/entrypoint.d/
