FROM digitalrebar/base
MAINTAINER Victor Lowther <victor@rackn.com>

ARG DR_TAG=latest
ENV DR_TAG ${DR_TAG}

RUN mkdir -p /tmp/consul /etc/consul.d  && \
    curl -fgL -o consul.zip \
        https://releases.hashicorp.com/consul/0.6.4/consul_0.6.4_linux_amd64.zip && \
    unzip consul.zip -d /usr/local/bin && \
    rm consul.zip

RUN mkdir -p linux/amd64 && \
    curl -fgL https://s3-us-west-2.amazonaws.com/rebar-bins/${DR_TAG}/linux/amd64/sign-it \
         -o linux/amd64/sign-it && \
    curl -fgL https://s3-us-west-2.amazonaws.com/rebar-bins/${DR_TAG}/sha256sums \
         -o rebar-bins.sha256sum && \
    (grep "linux/amd64/sign-it" rebar-bins.sha256sum | sha256sum -c && rm rebar-bins.sha256sum) && \
    mv linux/amd64/sign-it /usr/local/bin/sign-it && \
    chmod 755 /usr/local/bin/sign-it && \
    rm -rf linux

COPY entrypoint.d/*.sh /usr/local/entrypoint.d/

