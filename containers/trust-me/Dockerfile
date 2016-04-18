FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

ENV SERVICE_NAME trust-me

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

RUN /usr/local/go/bin/go get -u github.com/cloudflare/cfssl/cmd/mkbundle \
  && /usr/local/go/bin/go get -u github.com/cloudflare/cfssl/cmd/multirootca \
  && cp $GOPATH/bin/* /usr/local/bin

COPY entrypoint.d/*.sh /usr/local/entrypoint.d/

