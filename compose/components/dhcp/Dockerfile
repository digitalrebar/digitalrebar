#FROM ubuntu:14.04
FROM compose_consul

ENV GOPATH /go

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

# Get Latest Go
RUN apt-get -y update \
  && apt-get -y install git make cmake curl openssh-server build-essential jq dhcp3-server \
  && curl -fgL -o '/tmp/goball.tgz' 'https://storage.googleapis.com/golang/go1.4.linux-amd64.tar.gz' \
  && rm -rf /usr/local/go \
  && tar -C '/usr/local' -zxf /tmp/goball.tgz \
  && rm /tmp/goball.tgz \
  && /usr/local/go/bin/go get -u github.com/digitalrebar/rebar-api/rebar \
  && cp -r $GOPATH/bin/rebar /usr/local/bin \
  && rm -rf /usr/local/go \
  && curl -fgL -o '/tmp/chef.deb' 'http://opscode-omnibus-packages.s3.amazonaws.com/ubuntu/12.04/x86_64/chef_11.12.8-1_amd64.deb' \
  && dpkg -i /tmp/chef.deb \
  && rm -f /tmp/chef.deb \
  && rm -rf $GOPATH \
  && curl -fgL -o '/root/dhcp-4.3.2.tar.gz' 'http://opencrowbar.s3-website-us-east-1.amazonaws.com/dhcp-4.3.2.tar.gz' \
  && cd /root \
  && tar -zxvf /root/dhcp-4.3.2.tar.gz

COPY discover.c '/root/dhcp-4.3.2/common/discover.c'
COPY dhcpd.h '/root/dhcp-4.3.2/includes/dhcpd.h'

RUN cd '/root/dhcp-4.3.2' \
 && ./configure \
      --enable-dhcpv6 \
      --with-srv-lease-file=/var/lib/dhcp/dhcpd.leases \
      --with-srv6-lease-file=/var/lib/dhcp/dhcpd6.leases \
      --with-cli-lease-file=/var/lib/dhclient/dhclient.leases \
      --with-cli6-lease-file=/var/lib/dhclient/dhclient6.leases \
      --with-srv-pid-file=/var/run/dhcpd.pid \
      --with-srv6-pid-file=/var/run/dhcpd6.pid \
      --with-cli-pid-file=/var/run/dhclient.pid \
      --with-cli6-pid-file=/var/run/dhclient6.pid \
      --with-relay-pid-file=/var/run/dhcrelay.pid \
      --with-relay6-pid-file=/var/run/dhcrelay6.pid \
      --enable-paranoia \
 &&  make \
 && cp /usr/sbin/dhcpd /usr/sbin/dhcpd.orig \
 && cp server/dhcpd /usr/sbin/dhcpd

COPY docker-entrypoint.sh /sbin/docker-entrypoint.sh

