FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

# Get packages
RUN curl -fgL https://s3-us-west-2.amazonaws.com/rebar-dhcp/rebar-dhcp \
         -o /usr/local/bin/rebar-dhcp && \
    chmod 755 /usr/local/bin/rebar-dhcp && \
    curl -fgL https://s3-us-west-2.amazonaws.com/rebar-dhcp/rebar-dhcp.sha256sum \
         -o /usr/local/bin/rebar-dhcp.sha256sum && \
    (cd /usr/local/bin && sha256sum -c rebar-dhcp.sha256sum && rm rebar-dhcp.sha256sum)

COPY entrypoint.d/*.sh /usr/local/entrypoint.d/
