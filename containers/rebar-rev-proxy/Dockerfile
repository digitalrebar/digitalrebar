FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

ENV SERVICE_NAME rev-proxy

ARG DR_TAG=latest
ENV DR_TAG ${DR_TAG}

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

RUN apt-get -y update && apt-get install -y xmlsec1

# Get packages
RUN mkdir -p linux/amd64 && \
    curl -fgL https://s3-us-west-2.amazonaws.com/rebar-bins/${DR_TAG}/linux/amd64/rebar-rev-proxy \
         -o linux/amd64/rebar-rev-proxy && \
    curl -fgL https://s3-us-west-2.amazonaws.com/rebar-bins/${DR_TAG}/sha256sums \
         -o rebar-bins.sha256sum && \
    (grep linux/amd64/rebar-rev-proxy rebar-bins.sha256sum | sha256sum -c && rm rebar-bins.sha256sum) && \
    mv linux/amd64/rebar-rev-proxy /usr/local/bin/rebar-rev-proxy && \
    chmod 755 /usr/local/bin/rebar-rev-proxy && \
    rm -rf linux

RUN apt-get -y install nodejs nodejs-legacy npm \
  && npm install -g bower \
  && npm install cssnano-cli html-minifier uglify-js \
  && npm install -g n \
  && n stable

COPY entrypoint.d/*.sh /usr/local/entrypoint.d/

