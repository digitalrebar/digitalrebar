FROM ubuntu:14.04
MAINTAINER Victor Lowther <victor@rackn.com>

ENV GOPATH /go

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

# Get packages
RUN apt-get update \
  && apt-get -y install jq curl unzip git \
  && curl -fgL -o '/tmp/goball.tgz' 'https://storage.googleapis.com/golang/go1.5.1.linux-amd64.tar.gz' \
  && tar -C '/usr/local' -zxf /tmp/goball.tgz && rm /tmp/goball.tgz \
  && /usr/local/go/bin/go get -v github.com/rackn/rebar-dhcp \
  && mkdir -p /var/cache/rebar-dhcp \
  && mkdir -p /etc/consul.d \
  && chmod 700 /var/cache/rebar-dhcp \
  && cp $GOPATH/bin/rebar-dhcp /usr/local/bin \
  && curl -fgL -o /usr/local/bin/rebar \
         https://s3-us-west-2.amazonaws.com/rebar-cli/rebar-linux-amd64 \
  && chmod 755 /usr/local/bin/rebar

COPY docker-entrypoint.sh /sbin/docker-entrypoint.sh
COPY entrypoint.d/*.sh /usr/local/entrypoint.d/
COPY rebar-dhcp.conf /etc/rebar-dhcp.conf
COPY rebar-dhcp-cert.conf /etc/rebar-dhcp-cert.conf

