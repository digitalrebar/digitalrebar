FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

ENV SERVICE_NAME rule-engine
ENV APIPORT 19202

ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

RUN curl -fgL -o /usr/local/bin/rule-engine           \
         http://rackn.s3-website-us-east-1.amazonaws.com/classifier/rule-engine-linux-amd64 && \
    chmod 755 /usr/local/bin/rule-engine

COPY entrypoint.d/*.sh /usr/local/entrypoint.d/