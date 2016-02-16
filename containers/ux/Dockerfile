FROM digitalrebar/deploy-service-wrapper
MAINTAINER Greg Althaus <greg@rackn.com>
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

RUN apt-get -y update \
  && apt-get -y install nodejs nodejs-legacy npm \
  && npm install -g bower

COPY entrypoint.d/*.sh /usr/local/entrypoint.d/