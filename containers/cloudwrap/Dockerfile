FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]
COPY entrypoint.d/*.sh /usr/local/entrypoint.d/
COPY cloudwrap /opt/cloudwrap

RUN apt-get -y update && \
    apt-get -y install software-properties-common wget && \
    apt-add-repository ppa:brightbox/ruby-ng && \
    add-apt-repository "deb http://archive.ubuntu.com/ubuntu $(lsb_release -sc)-backports main restricted universe multiverse" && \
    apt-get -y update && \
    apt-get -y install ruby2.2 ruby2.2-dev make cmake curl build-essential jq libxml2-dev libcurl4-openssl-dev libssl-dev && \
    gem install bundler && \
    mkdir -p /var/cache/cloudwrap/gems && \
    (cd /opt/cloudwrap && bundle install --path /var/cache/cloudwrap/gems --standalone --binstubs /usr/local/bin)
