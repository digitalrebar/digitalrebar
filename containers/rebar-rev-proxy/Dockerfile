FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

ENV SERVICE_NAME rev-proxy

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

COPY entrypoint.d/*.sh /usr/local/entrypoint.d/

COPY rebar-rev-proxy $GOPATH/src/github.com/rackn/rebar-rev-proxy
RUN /usr/local/go/bin/go get -u github.com/hashicorp/consul/api \
    && /usr/local/go/bin/go get -u github.com/RobotsAndPencils/go-saml \
    && cd "$GOPATH/src/github.com/rackn/rebar-rev-proxy" \
    && /usr/local/go/bin/go build && cp rebar-rev-proxy /usr/local/bin
