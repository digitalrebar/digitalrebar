FROM ubuntu:14.04
MAINTAINER Victor Lowther <victor@rackn.com>

ENV GOPATH /go

ARG DR_TAG=latest
ENV DR_TAG ${DR_TAG}

# Get packages
RUN mkdir -p /usr/local/sbin/ /usr/local/entrypoint.d /etc/rebar-data && \
    apt-get update && \
    apt-get -y dist-upgrade && \
    apt-get install -y curl unzip git jq build-essential && \
    curl -fgL -o '/tmp/goball.tgz' \
         'https://storage.googleapis.com/golang/go1.7.linux-amd64.tar.gz' && \
    tar -C '/usr/local' -zxf /tmp/goball.tgz && rm /tmp/goball.tgz

RUN mkdir -p linux/amd64 && \
    curl -fgL https://s3-us-west-2.amazonaws.com/rebar-bins/${DR_TAG}/linux/amd64/rebar \
         -o linux/amd64/rebar && \
    curl -fgL https://s3-us-west-2.amazonaws.com/rebar-bins/${DR_TAG}/sha256sums \
         -o rebar-bins.sha256sum && \
    (grep "linux/amd64/rebar\$" rebar-bins.sha256sum | sha256sum -c && rm rebar-bins.sha256sum) && \
    mv linux/amd64/rebar /usr/local/bin/rebar && \
    chmod 755 /usr/local/bin/rebar && \
    rm -rf linux

COPY docker-entrypoint.sh /sbin/docker-entrypoint.sh
