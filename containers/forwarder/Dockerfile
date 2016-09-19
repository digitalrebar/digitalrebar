FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

ARG DR_TAG=latest
ENV DR_TAG ${DR_TAG}

ENV GOPATH /go

RUN mkdir -p linux/amd64 && \
    curl -fgL https://s3-us-west-2.amazonaws.com/rebar-bins/${DR_TAG}/linux/amd64/forwarder \
         -o linux/amd64/forwarder && \
    curl -fgL https://s3-us-west-2.amazonaws.com/rebar-bins/${DR_TAG}/sha256sums \
         -o rebar-bins.sha256sum && \
    (grep "linux/amd64/forwarder\$" rebar-bins.sha256sum | sha256sum -c && rm rebar-bins.sha256sum) && \
    mv linux/amd64/forwarder /usr/local/bin/forwarder && \
    chmod 755 /usr/local/bin/forwarder && \
    rm -rf linux

RUN apt-get install -y iptables ntp ntpdate

COPY entrypoint.d/*.sh /usr/local/entrypoint.d/
COPY ntp.conf /etc/ntp.conf

ENTRYPOINT ["/sbin/docker-entrypoint.sh"]
