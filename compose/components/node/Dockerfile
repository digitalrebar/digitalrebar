FROM ubuntu:14.04

# TORUN

ENV GOPATH /go

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

# Get packages
RUN apt-get update \
  && apt-get -y install git make build-essential jq openssh-server curl \
  && curl -fgL -o '/tmp/goball.tgz' 'https://storage.googleapis.com/golang/go1.4.linux-amd64.tar.gz' \
  && rm -rf /usr/local/go \
  && tar -C '/usr/local' -zxf /tmp/goball.tgz \
  && rm /tmp/goball.tgz \
  && /usr/local/go/bin/go get -u github.com/digitalrebar/rebar-api/rebar \
  && rm -rf $GOPATH/pkg \
  && cp -r $GOPATH/bin/rebar /usr/local/bin \
  && rm -rf $GOPATH /usr/local/go \
  && apt-get -y purge git make build-essential \
  && curl -fgL -o '/tmp/chef.deb' 'http://opscode-omnibus-packages.s3.amazonaws.com/ubuntu/12.04/x86_64/chef_11.12.8-1_amd64.deb' \
  && dpkg -i /tmp/chef.deb \
  && rm -f /tmp/chef.deb

COPY docker-entrypoint.sh /sbin/docker-entrypoint.sh

