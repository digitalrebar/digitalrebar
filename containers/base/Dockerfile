FROM ubuntu:14.04
MAINTAINER Victor Lowther <victor@rackn.com>

ENV GOPATH /go

COPY docker-entrypoint.sh /sbin/docker-entrypoint.sh

RUN apt-get update && apt-get -y dist-upgrade && apt-get install -y curl unzip git && \
  mkdir -p /tmp/consul /ui /etc/consul.d /usr/local/sbin/ /usr/local/entrypoint.d && \
  curl -fgL -o consul.zip https://dl.bintray.com/mitchellh/consul/0.5.2_linux_amd64.zip && \
  curl -fgL -o consul_ui.zip https://dl.bintray.com/mitchellh/consul/0.5.2_web_ui.zip && \
  unzip consul.zip -d /usr/local/bin && unzip consul_ui.zip -d /ui && \
  rm consul.zip consul_ui.zip && \
  curl -fgL -o '/tmp/goball.tgz' 'https://storage.googleapis.com/golang/go1.5.1.linux-amd64.tar.gz' && \
  tar -C '/usr/local' -zxf /tmp/goball.tgz && rm /tmp/goball.tgz && \
  curl -fgL -o '/tmp/chef.deb' \
       'https://opscode-omnibus-packages.s3.amazonaws.com/ubuntu/10.04/x86_64/chef_11.18.12-1_amd64.deb' && \
  dpkg -i /tmp/chef.deb && rm -f /tmp/chef.deb


