FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

ARG DR_TAG
RUN apt-get update \
    && apt-get install -y ntp \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

COPY entrypoint.d/*.sh /usr/local/entrypoint.d/ 

ENTRYPOINT ["/sbin/docker-entrypoint.sh"]
