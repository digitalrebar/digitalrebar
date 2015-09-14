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
  && apt-get -y install bind9 bind9utils dnsutils \
  && curl -fgL -o '/tmp/goball.tgz' 'https://storage.googleapis.com/golang/go1.4.linux-amd64.tar.gz' \
  && rm -rf /usr/local/go \
  && tar -C '/usr/local' -zxf /tmp/goball.tgz \
  && rm /tmp/goball.tgz \
  && /usr/local/go/bin/go get -v github.com/rackn/rebar-dns-mgmt \
  && rm -rf $GOPATH/pkg \
  && mkdir -p /etc/dns-mgmt.d \
  && mkdir -p /var/cache/rebar-dns-mgmt \
  && mkdir -p /etc/consul.d \
  && chmod 700 /var/cache/rebar-dns-mgmt \
  && cp -r $GOPATH/src/github.com/rackn/rebar-dns-mgmt/*.tmpl /etc/dns-mgmt.d \
  && cp $GOPATH/bin/rebar-dns-mgmt /usr/local/bin/rebar-dns-mgmt \
  && rm -rf $GOPATH /usr/local/go \
  && apt-get -y purge git make build-essential

COPY bind /etc/bind

COPY docker-entrypoint.sh /sbin/docker-entrypoint.sh

COPY dns.json /etc/consul.d/dns.json
COPY dns-mgmt.json /etc/consul.d/dns-mgmt.json
COPY dns-mgmt.conf /etc/dns-mgmt.conf
COPY database.json /var/cache/rebar-dns-mgmt/database.json
COPY dns-mgmt-cert.conf /etc/dns-mgmt-cert.conf

# Ports to expose/map
# EXPOSE 53, 6754
