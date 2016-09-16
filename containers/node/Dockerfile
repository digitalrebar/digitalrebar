FROM ubuntu:14.04

# TORUN

ENV GOPATH /go

ARG DR_TAG=latest
ENV DR_TAG ${DR_TAG}

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

# Get packages
RUN apt-get update \
  && apt-get -y install git jq openssh-server curl \
  && curl -fgL -o '/tmp/chef.deb' 'http://opscode-omnibus-packages.s3.amazonaws.com/ubuntu/12.04/x86_64/chef_11.12.8-1_amd64.deb' \
  && dpkg -i /tmp/chef.deb \
  && rm -f /tmp/chef.deb

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

