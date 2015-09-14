FROM ubuntu:14.04

RUN apt-get update && apt-get install -y curl unzip \
  && mkdir -p /tmp/consul /ui \
  && curl -fL -o consul.zip https://dl.bintray.com/mitchellh/consul/0.5.2_linux_amd64.zip \
  && curl -fL -o consul_ui.zip https://dl.bintray.com/mitchellh/consul/0.5.2_web_ui.zip \
  && unzip consul.zip -d /usr/local/bin \
  && unzip consul_ui.zip -d /ui \
  && rm consul.zip consul_ui.zip \
  && mkdir -p /etc/consul.d \
  && apt-get purge -y unzip

# TORUN

ENV GOPATH /go

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

# Get packages
RUN apt-get update \
  && apt-get -y install git make build-essential jq \
  && curl -fgL -o '/tmp/goball.tgz' 'https://storage.googleapis.com/golang/go1.4.linux-amd64.tar.gz' \
  && rm -rf /usr/local/go \
  && tar -C '/usr/local' -zxf /tmp/goball.tgz \
  && rm /tmp/goball.tgz \
  && /usr/local/go/bin/go get -v github.com/rackn/rebar-dhcp \
  && mkdir -p /var/cache/rebar-dhcp \
  && mkdir -p /etc/consul.d \
  && chmod 700 /var/cache/rebar-dhcp \
  && cp $GOPATH/bin/rebar-dhcp /usr/local/bin \
  && rm -rf $GOPATH /usr/local/go \
  && apt-get -y purge git make build-essential

COPY docker-entrypoint.sh /sbin/docker-entrypoint.sh

COPY dhcp.json /etc/consul.d/dhcp.json
COPY rebar-dhcp.conf /etc/rebar-dhcp.conf
COPY database.json /var/cache/rebar-dhcp/database.json
COPY rebar-dhcp-cert.conf /etc/rebar-dhcp-cert.conf

# Ports to expose/map
# EXPOSE 67, 6755
