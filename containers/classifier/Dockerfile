FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

ENV SERVICE_NAME classifier

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

RUN curl -fgL -o /usr/local/bin/classifier \
         http://rackn.s3-website-us-east-1.amazonaws.com/classifier/classifier-linux-amd64 && \
    chmod 755 /usr/local/bin/classifier

COPY entrypoint.d/*.sh /usr/local/entrypoint.d/

