FROM ubuntu:14.04
MAINTAINER Victor Lowther <victor@rackn.com>

ENV GOPATH /go

COPY docker-entrypoint.sh /sbin/docker-entrypoint.sh

RUN apt-get update && apt-get -y dist-upgrade && apt-get install -y curl unzip git && \
  mkdir -p /tmp/consul /ui /etc/consul.d /usr/local/sbin/ /usr/local/entrypoint.d && \
  curl -fgL -o consul.zip https://releases.hashicorp.com/consul/0.6.3/consul_0.6.3_linux_amd64.zip && \
  curl -fgL -o consul_ui.zip https://releases.hashicorp.com/consul/0.6.3/consul_0.6.3_web_ui.zip && \
  unzip consul.zip -d /usr/local/bin && unzip consul_ui.zip -d /ui && \
  rm consul.zip consul_ui.zip && \
  curl -fgL -o '/tmp/goball.tgz' 'https://storage.googleapis.com/golang/go1.5.3.linux-amd64.tar.gz' && \
  tar -C '/usr/local' -zxf /tmp/goball.tgz && rm /tmp/goball.tgz


