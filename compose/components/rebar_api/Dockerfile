#FROM ubuntu:14.04
FROM compose_consul

ENV GOPATH /go

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

RUN groupadd -r crowbar && useradd -r -u 1000 -g crowbar -p '$6$afAL.34B$T2WR6zycEe2q3DktVtbH2orOroblhR6uCdo5n3jxLsm47PBm9lwygTbv3AjcmGDnvlh0y83u2yprET8g9/mve.' -m -d /home/crowbar -s /bin/bash crowbar

# Get a good ruby
RUN apt-get -y install software-properties-common wget \
  && apt-add-repository ppa:brightbox/ruby-ng \
  && add-apt-repository "deb http://archive.ubuntu.com/ubuntu $(lsb_release -sc)-backports main restricted universe multiverse" \
  && add-apt-repository "deb http://apt.postgresql.org/pub/repos/apt/ trusty-pgdg main" \
  && wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add - \
  && apt-get -y update \
  && apt-get -y install ruby2.1 ruby2.1-dev git make cmake curl swig3.0 libcimcclient0-dev libxml2-dev libcurl4-openssl-dev libssl-dev build-essential jq sudo libopenwsman-dev postgresql-9.4 postgresql-client-9.4 libpq5 libpq-dev autoconf \
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
RUN /usr/local/go/bin/go get -u github.com/VictorLowther/crowbar-api/crowbar \
  && cp "$GOPATH/bin/crowbar" /usr/local/bin \
  && rm -rf /usr/local/go \
  && rm -rf $GOPATH

# Get Latest OCB
RUN mkdir -p /opt/opencrowbar
COPY digitalrebar/ /opt/opencrowbar

# Make dirs
RUN mkdir -p /var/run/crowbar \
  && mkdir -p /var/cache/crowbar/cookbooks \
  && mkdir -p /var/cache/crowbar/gems \
  && mkdir -p /var/cache/crowbar/bin \
  && mkdir -p /var/log/crowbar \
  && mkdir -p /etc/sudoers.d \
  && mkdir -p /home/crowbar/.ssh \
  && chown -R crowbar:crowbar /opt/opencrowbar \
  && chown -R crowbar:crowbar /var/run/crowbar \
  && chown -R crowbar:crowbar /var/cache/crowbar \
  && chown -R crowbar:crowbar /var/log/crowbar \
  && chown -R crowbar:crowbar /home/crowbar/.ssh \
  && chmod 755 /home/crowbar/.ssh

RUN ln -s /usr/bin/swig3.0 /usr/bin/swig

# Build gems
RUN su -l -c 'cd /opt/opencrowbar/core/rails; bundle install --path /var/cache/crowbar/gems --standalone --binstubs /var/cache/crowbar/bin' crowbar \
  && ln -s /var/cache/crowbar/bin/puma /usr/bin/puma \
  && ln -s /var/cache/crowbar/bin/pumactl /usr/bin/pumactl

COPY docker-entrypoint.sh /sbin/docker-entrypoint.sh
COPY crowbar_sudoer /etc/sudoers.d/crowbar
COPY ssh_config /home/crowbar/.ssh/config

RUN chown crowbar:crowbar /home/crowbar/.ssh/config \
  && chmod 644 /home/crowbar/.ssh/config

RUN mkdir -p /etc/consul.d
COPY crowbar-api.json /etc/consul.d/crowbar-api.json
COPY crowbar-runner.json /etc/consul.d/crowbar-runner.json

# These are the ports being exposed for this system
# We don't want other systems inheriting this
# EXPOSE 3000
