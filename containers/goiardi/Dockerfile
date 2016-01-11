FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

# Get packages
RUN apt-get update && \
    apt-get -y install make build-essential cpanminus perl perl-doc libdbd-pg-perl \
                       coreutils postgresql && \
     /usr/local/go/bin/go get -v github.com/ctdk/goiardi && \
     cpanm --quiet --notest App::Sqitch && \
     mkdir -p /etc/goiardi /var/cache/goiardi && \
     apt-get -y purge make build-essential

COPY goiardi.conf /etc/goiardi/
COPY entrypoint.d/*.sh /usr/local/entrypoint.d/
COPY goiardi-internal.json /root/goiardi-internal.json
COPY goiardi-external.json /root/goiardi-external.json
