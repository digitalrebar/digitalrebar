FROM ubuntu:14.04
MAINTAINER Victor Lowther <victor@rackn.com>

ENV GOPATH /go

RUN mkdir -p /usr/local/sbin/ /usr/local/entrypoint.d /etc/rebar-data && \
    apt-get update && \
    apt-get -y dist-upgrade && \
    apt-get install -y curl unzip git jq && \
    curl -fgL -o /usr/local/bin/rebar \
         https://s3-us-west-2.amazonaws.com/rebar-cli/rebar-linux-amd64 && \
    chmod 755 /usr/local/bin/rebar && \
    curl -fgL -o '/tmp/goball.tgz' \
         'https://storage.googleapis.com/golang/go1.5.3.linux-amd64.tar.gz' && \
    tar -C '/usr/local' -zxf /tmp/goball.tgz && rm /tmp/goball.tgz

COPY docker-entrypoint.sh /sbin/docker-entrypoint.sh
