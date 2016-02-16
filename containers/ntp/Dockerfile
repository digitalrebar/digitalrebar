FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

RUN apt-get install -y ntp

COPY entrypoint.d/*.sh /usr/local/entrypoint.d/ 

ENTRYPOINT ["/sbin/docker-entrypoint.sh"]
