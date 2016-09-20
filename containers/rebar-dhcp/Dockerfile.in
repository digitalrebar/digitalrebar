FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

ARG DR_TAG
ADD http://localhost:28569/${DR_TAG}/linux/amd64/rebar-dhcp /usr/local/bin/
RUN chmod 755 /usr/local/bin/*
COPY entrypoint.d/*.sh /usr/local/entrypoint.d/
