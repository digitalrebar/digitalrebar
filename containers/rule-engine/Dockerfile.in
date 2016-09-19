FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

ENV SERVICE_NAME rule-engine
ENV APIPORT 19202

ARG DR_TAG

ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

ADD http://localhost:28569/${DR_TAG}/linux/amd64/rule-engine /usr/local/bin/
RUN chmod 755 /usr/local/bin/*

COPY entrypoint.d/*.sh /usr/local/entrypoint.d/
