FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

ARG DR_TAG=latest
ENV DR_TAG ${DR_TAG}

RUN apt-get install -y ntp

COPY entrypoint.d/*.sh /usr/local/entrypoint.d/ 

ENTRYPOINT ["/sbin/docker-entrypoint.sh"]
