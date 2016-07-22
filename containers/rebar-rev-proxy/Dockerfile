FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

ENV SERVICE_NAME rev-proxy

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

RUN apt-get -y update && apt-get install -y xmlsec1 \
    && /usr/local/go/bin/go get -u github.com/hashicorp/consul/api \
    && /usr/local/go/bin/go get -u github.com/RobotsAndPencils/go-saml \
    && /usr/local/go/bin/go get -u github.com/kmanley/go-http-auth \
    && /usr/local/go/bin/go get -u github.com/dgrijalva/jwt-go \
    && /usr/local/go/bin/go get -u github.com/digitalrebar/go-common/cert

RUN apt-get -y update \
  && apt-get -y install nodejs nodejs-legacy npm \
  && npm install -g bower \
  && npm install cssnano-cli html-minifier uglify-js \
  && npm install -g n \
  && n stable

COPY rebar-rev-proxy $GOPATH/src/github.com/rackn/rebar-rev-proxy

RUN cd "$GOPATH/src/github.com/rackn/rebar-rev-proxy" \
    && /usr/local/go/bin/go build && cp rebar-rev-proxy /usr/local/bin

COPY entrypoint.d/*.sh /usr/local/entrypoint.d/

