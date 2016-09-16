FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

ARG DR_TAG=latest
ENV DR_TAG ${DR_TAG}

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

RUN mkdir -p linux/amd64 && \
    curl -fgL https://s3-us-west-2.amazonaws.com/rebar-bins/${DR_TAG}/linux/amd64/rebar-dns-mgmt \
         -o linux/amd64/rebar-dns-mgmt && \
    curl -fgL https://s3-us-west-2.amazonaws.com/rebar-bins/${DR_TAG}/sha256sums \
         -o rebar-bins.sha256sum && \
    (grep "linux/amd64/rebar-dns-mgmt\$" rebar-bins.sha256sum | sha256sum -c && rm rebar-bins.sha256sum) && \
    mv linux/amd64/rebar-dns-mgmt /usr/local/bin/rebar-dns-mgmt && \
    chmod 755 /usr/local/bin/rebar-dns-mgmt && \
    rm -rf linux

# Get packages
RUN apt-get update \
  && apt-get -y install bind9 bind9utils dnsutils \
  && mkdir -p /etc/dns-mgmt.d \
  && mkdir -p /var/cache/rebar-dns-mgmt \
  && chmod 700 /var/cache/rebar-dns-mgmt

# GREG FIx this in config
#  && cp -r $GOPATH/src/github.com/rackn/rebar-dns-mgmt/*.tmpl /etc/dns-mgmt.d \

COPY bind /etc/bind
COPY entrypoint.d/*.sh /usr/local/entrypoint.d/
