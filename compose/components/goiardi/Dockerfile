#FROM ubuntu:14.04
FROM compose_consul

# TORUN

ENV GOPATH /go

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

VOLUME /etc/goiardi/data

# Get packages
RUN apt-get update \
  && apt-get -y install git make build-essential cpanminus perl perl-doc libdbd-pg-perl jq coreutils postgresql

# Get Latest Go
RUN curl -fgL -o '/tmp/goball.tgz' 'https://storage.googleapis.com/golang/go1.4.linux-amd64.tar.gz'
RUN rm -rf /usr/local/go 
RUN tar -C '/usr/local' -zxf /tmp/goball.tgz 
RUN rm /tmp/goball.tgz
RUN curl -fgL -o '/tmp/chef.deb' 'http://opscode-omnibus-packages.s3.amazonaws.com/ubuntu/12.04/x86_64/chef_11.12.8-1_amd64.deb'
RUN dpkg -i /tmp/chef.deb
RUN rm -f /tmp/chef.deb

# Install the packages we need, clean up after them and us
RUN /usr/local/go/bin/go get -v github.com/ctdk/goiardi \
 && rm -rf $GOPATH/pkg

RUN cpanm --quiet --notest App::Sqitch

RUN mkdir -p /etc/goiardi
RUN mkdir -p /var/cache/goiardi

RUN apt-get -y purge git make build-essential

COPY goiardi.conf /etc/goiardi/
COPY docker-entrypoint.sh /sbin/docker-entrypoint.sh

RUN mkdir -p /etc/consul.d
COPY goiardi.json /etc/consul.d/goiardi.json

# Expose or map port
# EXPOST 4646
