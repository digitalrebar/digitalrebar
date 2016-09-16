FROM digitalrebar/base
MAINTAINER Victor Lowther <victor@rackn.com>

ARG DR_TAG=latest
ENV DR_TAG ${DR_TAG}

RUN mkdir -p /tmp/consul /etc/consul.d  && \
    curl -fgL -o consul.zip \
        https://releases.hashicorp.com/consul/0.6.3/consul_0.6.3_linux_amd64.zip && \
    unzip consul.zip -d /usr/local/bin && \
    rm consul.zip

COPY entrypoint.d/*.sh /usr/local/entrypoint.d/

