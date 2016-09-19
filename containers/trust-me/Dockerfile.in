FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

ENV SERVICE_NAME trust-me
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

ARG DR_TAG

ADD http://localhost:28569/${DR_TAG}/linux/amd64/trust-me /usr/local/bin/
ADD http://localhost:28569/${DR_TAG}/linux/amd64/sign-it /usr/local/bin/
RUN chmod 755 /usr/local/bin/*

COPY test_tm_up.sh /usr/local/bin/test_tm_up.sh
RUN chmod 755 /usr/local/bin/test_tm_up.sh
COPY entrypoint.d/*.sh /usr/local/entrypoint.d/

