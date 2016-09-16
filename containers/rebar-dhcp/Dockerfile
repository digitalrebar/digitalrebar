FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

ARG DR_TAG=latest
ENV DR_TAG ${DR_TAG}

# Get packages
RUN mkdir -p linux/amd64 && \
    curl -fgL https://s3-us-west-2.amazonaws.com/rebar-bins/${DR_TAG}/linux/amd64/rebar-dhcp \
         -o linux/amd64/rebar-dhcp && \
    curl -fgL https://s3-us-west-2.amazonaws.com/rebar-bins/${DR_TAG}/sha256sums \
         -o rebar-bins.sha256sum && \
    (grep linux/amd64/rebar-dhcp rebar-bins.sha256sum | sha256sum -c && rm rebar-bins.sha256sum) && \
    mv linux/amd64/rebar-dhcp /usr/local/bin/rebar-dhcp && \
    chmod 755 /usr/local/bin/rebar-dhcp && \
    rm -rf linux

COPY entrypoint.d/*.sh /usr/local/entrypoint.d/
