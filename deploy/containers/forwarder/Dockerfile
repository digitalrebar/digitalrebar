FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

ENV GOPATH /go

RUN apt-get install -y iptables ntp ntpdate && \
  /usr/local/go/bin/go get -u github.com/digitalrebar/forwarder && \
  cp -r $GOPATH/bin/forwarder /usr/local/bin

COPY entrypoint.d/*.sh /usr/local/entrypoint.d/
COPY ntp.conf /etc/ntp.conf

ENTRYPOINT ["/sbin/docker-entrypoint.sh"]
