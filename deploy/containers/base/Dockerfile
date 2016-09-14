FROM ubuntu:14.04
MAINTAINER Victor Lowther <victor@rackn.com>

ENV GOPATH /go

RUN mkdir -p /usr/local/sbin/ /usr/local/entrypoint.d /etc/rebar-data && \
    apt-get update && \
    apt-get -y dist-upgrade && \
    apt-get install -y curl unzip git jq build-essential && \
    curl -fgL -o /usr/local/bin/rebar \
         https://s3-us-west-2.amazonaws.com/rebar-cli/rebar-linux-amd64 && \
    chmod 755 /usr/local/bin/rebar && \
    curl -fgL -o '/tmp/goball.tgz' \
         'https://storage.googleapis.com/golang/go1.7.linux-amd64.tar.gz' && \
    tar -C '/usr/local' -zxf /tmp/goball.tgz && rm /tmp/goball.tgz && \
    /usr/local/go/bin/go get -u github.com/cloudflare/cfssl/cmd/cfssl && \
    /usr/local/go/bin/go get -u github.com/cloudflare/cfssl/cmd/cfssljson && \
    cp $GOPATH/bin/* /usr/local/bin

COPY docker-entrypoint.sh /sbin/docker-entrypoint.sh
