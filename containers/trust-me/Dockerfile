FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

ENV SERVICE_NAME trust-me

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

RUN /usr/local/go/bin/go get -u github.com/digitalrebar/certificates/trust-me \
  && /usr/local/go/bin/go get -u github.com/digitalrebar/certificates/sign-it \
  && cp $GOPATH/bin/* /usr/local/bin

COPY test_tm_up.sh /usr/local/bin/test_tm_up.sh
RUN chmod 755 /usr/local/bin/test_tm_up.sh
COPY entrypoint.d/*.sh /usr/local/entrypoint.d/

