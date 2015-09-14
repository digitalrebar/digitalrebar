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

ENV GOPATH /go

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

# Get Latest Go
RUN apt-get -y update \
  && apt-get -y install git make cmake curl openssh-server build-essential jq bsdtar createrepo \
  && apt-get -y install tftpd-hpa \
  && curl -fgL -o '/tmp/goball.tgz' 'https://storage.googleapis.com/golang/go1.4.linux-amd64.tar.gz' \
  && rm -rf /usr/local/go \
  && tar -C '/usr/local' -zxf /tmp/goball.tgz \
  && rm /tmp/goball.tgz \
  && /usr/local/go/bin/go get -u github.com/VictorLowther/sws \
  && /usr/local/go/bin/go get -u github.com/digitalrebar/rebar-api/rebar \
  && cp -r $GOPATH/bin/rebar /usr/local/bin \
  && cp "$GOPATH/bin/sws" /usr/local/bin \
  && rm -rf /usr/local/go \
  && curl -fgL -o '/tmp/chef.deb' 'http://opscode-omnibus-packages.s3.amazonaws.com/ubuntu/12.04/x86_64/chef_11.12.8-1_amd64.deb' \
  && dpkg -i /tmp/chef.deb \
  && rm -f /tmp/chef.deb \
  && rm -rf $GOPATH \
  && apt-get -y purge git make cmake build-essential

COPY docker-entrypoint.sh /sbin/docker-entrypoint.sh
COPY tftpd.conf /etc/default/tftpd-hpa

RUN mkdir -p /etc/consul.d

# These are the ports being exposed for this system
# We don't want other systems inheriting this
# EXPOSE 3000
