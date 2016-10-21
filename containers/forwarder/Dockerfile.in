FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

ARG DR_TAG
RUN apt-get update \
  && apt-get install -y --no-install-recommends iptables ntp ntpdate \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

ADD http://localhost:28569/${DR_TAG}/linux/amd64/forwarder /usr/local/bin/forwarder
COPY entrypoint.d/*.sh /usr/local/entrypoint.d/
COPY ntp.conf /etc/ntp.conf
RUN chmod 755 /usr/local/bin/forwarder

ENTRYPOINT ["/sbin/docker-entrypoint.sh"]
