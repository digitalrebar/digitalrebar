FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

COPY entrypoint.d/*.sh /usr/local/entrypoint.d/

RUN  apt-get -y update && \
     apt-get -y install openssh-server make build-essential