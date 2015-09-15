FROM ubuntu:14.04

RUN apt-get update && apt-get install -y curl unzip \
  && mkdir -p /tmp/consul /ui \
  && curl -fL -o consul.zip https://dl.bintray.com/mitchellh/consul/0.5.2_linux_amd64.zip \
  && curl -fL -o consul_ui.zip https://dl.bintray.com/mitchellh/consul/0.5.2_web_ui.zip \
  && unzip consul.zip -d /usr/local/bin \
  && unzip consul_ui.zip -d /ui \
  && rm consul.zip consul_ui.zip \
  && mkdir -p /etc/consul.d \
  && apt-get purge -y unzip

ENV GOPATH /go

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

RUN groupadd -r rebar && useradd -r -u 1000 -g rebar -p '$6$afAL.34B$T2WR6zycEe2q3DktVtbH2orOroblhR6uCdo5n3jxLsm47PBm9lwygTbv3AjcmGDnvlh0y83u2yprET8g9/mve.' -m -d /home/rebar -s /bin/bash rebar

# Get a good ruby
RUN apt-get -y install software-properties-common wget \
  && apt-add-repository ppa:brightbox/ruby-ng \
  && add-apt-repository "deb http://archive.ubuntu.com/ubuntu $(lsb_release -sc)-backports main restricted universe multiverse" \
  && add-apt-repository "deb http://apt.postgresql.org/pub/repos/apt/ trusty-pgdg main" \
  && wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add - \
  && apt-get -y update \
  && apt-get -y install ruby2.1 ruby2.1-dev git make cmake curl swig3.0 libcimcclient0-dev libxml2-dev libcurl4-openssl-dev libssl-dev build-essential jq sudo libopenwsman-dev postgresql-9.4 postgresql-client-9.4 libpq5 libpq-dev autoconf uuid-runtime \
  && gem install bundler \
  && gem install net-http-digest_auth \
  && gem install berkshelf

# Get Latest Go
RUN curl -fgL -o '/tmp/goball.tgz' 'https://storage.googleapis.com/golang/go1.4.linux-amd64.tar.gz'
RUN rm -rf /usr/local/go
RUN tar -C '/usr/local' -zxf /tmp/goball.tgz
RUN rm /tmp/goball.tgz

RUN curl -fgL -o '/tmp/chef.deb' 'http://opscode-omnibus-packages.s3.amazonaws.com/ubuntu/12.04/x86_64/chef_11.12.8-1_amd64.deb'
RUN dpkg -i /tmp/chef.deb
RUN rm -f /tmp/chef.deb

# Install the packages we need, clean up after them and us
RUN /usr/local/go/bin/go get -u github.com/digitalrebar/rebar-api/rebar \
  && cp "$GOPATH/bin/rebar" /usr/local/bin \
  && rm -rf /usr/local/go \
  && rm -rf $GOPATH

# Get Latest OCB
RUN mkdir -p /opt/digitalrebar
COPY digitalrebar/ /opt/digitalrebar

RUN [ -d /opt/digitalrebar/core ] || git clone https://github.com/digitalrebar/core /opt/digitalrebar/core

# Make dirs
RUN mkdir -p /var/run/rebar \
  && mkdir -p /var/cache/rebar/cookbooks \
  && mkdir -p /var/cache/rebar/gems \
  && mkdir -p /var/cache/rebar/bin \
  && mkdir -p /var/log/rebar \
  && mkdir -p /etc/sudoers.d \
  && mkdir -p /home/rebar/.ssh \
  && chown -R rebar:rebar /opt/digitalrebar \
  && chown -R rebar:rebar /var/run/rebar \
  && chown -R rebar:rebar /var/cache/rebar \
  && chown -R rebar:rebar /var/log/rebar \
  && chown -R rebar:rebar /home/rebar/.ssh \
  && chmod 755 /home/rebar/.ssh

RUN ln -s /usr/bin/swig3.0 /usr/bin/swig

# Build gems
RUN su -l -c 'cd /opt/digitalrebar/core/rails; bundle install --path /var/cache/rebar/gems --standalone --binstubs /var/cache/rebar/bin' rebar \
  && ln -s /var/cache/rebar/bin/puma /usr/bin/puma \
  && ln -s /var/cache/rebar/bin/pumactl /usr/bin/pumactl

COPY docker-entrypoint.sh /sbin/docker-entrypoint.sh
COPY rebar_sudoer /etc/sudoers.d/rebar
COPY ssh_config /home/rebar/.ssh/config

RUN chown rebar:rebar /home/rebar/.ssh/config \
  && chmod 644 /home/rebar/.ssh/config

RUN mkdir -p /etc/consul.d
COPY rebar-api.json /etc/consul.d/rebar-api.json
COPY rebar-runner.json /etc/consul.d/rebar-runner.json

# These are the ports being exposed for this system
# We don't want other systems inheriting this
# EXPOSE 3000
