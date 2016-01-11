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
  && cd /tmp \
  && curl -fgL -O http://downloads.sourceforge.net/project/elilo/elilo/elilo-3.16/elilo-3.16-all.tar.gz \
  && curl -fgL -O https://www.kernel.org/pub/linux/utils/boot/syslinux/syslinux-6.03.tar.xz

COPY tftpd.conf /etc/default/tftpd-hpa
COPY entrypoint.d/*.sh /usr/local/entrypoint.d/
