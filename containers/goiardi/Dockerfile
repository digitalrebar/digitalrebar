FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

# Get packages
RUN apt-get update && \
    apt-get -y install make build-essential cpanminus perl perl-doc libdbd-pg-perl \
                       curl coreutils postgresql-client && \
     /usr/local/go/bin/go get -v github.com/ctdk/goiardi && \
     cpanm --quiet --notest App::Sqitch && \
     mkdir -p /etc/goiardi /var/cache/goiardi && \
     apt-get -y purge make build-essential && \
     curl -fgL -o '/tmp/chef.deb' \
       'https://opscode-omnibus-packages.s3.amazonaws.com/ubuntu/10.04/x86_64/chef_11.18.12-1_amd64.deb' && \
     dpkg -i /tmp/chef.deb && rm -f /tmp/chef.deb

COPY goiardi.conf /etc/goiardi/
COPY entrypoint.d/*.sh /usr/local/entrypoint.d/

