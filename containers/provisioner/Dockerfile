FROM digitalrebar/managed-service
MAINTAINER Victor Lowther <victor@rackn.com>

ENV SERVICE_NAME provisioner

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

# Get Latest Go
RUN apt-get -y update \
  && apt-get -y install cmake bsdtar createrepo tftpd-hpa \
  && /usr/local/go/bin/go get -u github.com/VictorLowther/sws \
  && cp "$GOPATH/bin/sws" /usr/local/bin \
  && apt-get -y purge make cmake build-essential \
  && curl -fgL -o /tmp/elilo.tar.gz \
     http://downloads.sourceforge.net/project/elilo/elilo/elilo-3.16/elilo-3.16-all.tar.gz \
  && curl -fgL -o /tmp/syslinux.tar.xz \
     https://www.kernel.org/pub/linux/utils/boot/syslinux/syslinux-6.03.tar.xz \
  && curl -fgL -o '/tmp/chef.deb' \
       'https://opscode-omnibus-packages.s3.amazonaws.com/ubuntu/10.04/x86_64/chef_11.18.12-1_amd64.deb' \
  && dpkg -i /tmp/chef.deb && rm -f /tmp/chef.deb

COPY *.json /root/
COPY tftpd.conf /etc/default/tftpd-hpa
COPY entrypoint.d/*.sh /usr/local/entrypoint.d/
COPY start-up.sh /tmp/

ENV TFTPROOT /tftpboot
ENV WEBPORT 8091
ENV APIPORT 8092