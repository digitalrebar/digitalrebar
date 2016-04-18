FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

# Get packages
RUN /usr/local/go/bin/go get -v github.com/rackn/rebar-dhcp \
  && mkdir -p /var/cache/rebar-dhcp /etc/consul.d \
  && chmod 700 /var/cache/rebar-dhcp \
  && cp $GOPATH/bin/rebar-dhcp /usr/local/bin

COPY entrypoint.d/*.sh /usr/local/entrypoint.d/
COPY rebar-dhcp.conf /etc/rebar-dhcp.conf
