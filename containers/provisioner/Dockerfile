FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

ENV SERVICE_NAME provisioner
ENV TFTPROOT /tftpboot
ENV WEBPORT 8091
ENV APIPORT 8092

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

# Get Latest Go
RUN apt-get -y update \
  && apt-get -y install cmake bsdtar createrepo tftpd-hpa xz-utils unzip bsdmainutils \
  && /usr/local/go/bin/go get -u github.com/VictorLowther/sws \
  && cp "$GOPATH/bin/sws" /usr/local/bin \
  && apt-get -y purge make cmake build-essential \
  && curl -fgL -o /tmp/elilo.tar.gz \
     http://downloads.sourceforge.net/project/elilo/elilo/elilo-3.16/elilo-3.16-all.tar.gz \
  && curl -fgL -o /tmp/syslinux.tar.xz \
     https://www.kernel.org/pub/linux/utils/boot/syslinux/syslinux-6.03.tar.xz \
  && curl -fgL -o /tmp/ipxe.efi http://boot.ipxe.org/ipxe.efi \
  && curl -fgL -o /tmp/ipxe.pxe http://boot.ipxe.org/ipxe.pxe \
  && curl -fgL -o /tmp/busybox https://www.busybox.net/downloads/binaries/busybox-x86_64

COPY tftpd.conf /etc/default/tftpd-hpa
COPY entrypoint.d/*.sh /usr/local/entrypoint.d/
COPY start-up.sh udhcpc_config stage1_init /tmp/

COPY update-nodes $GOPATH/src/github.com/rackn/provisioner-mgmt
RUN /usr/local/go/bin/go get -u github.com/hashicorp/consul/api \
                                github.com/VictorLowther/jsonpatch \
                                github.com/gin-gonic/gin \
                                github.com/satori/go.uuid \
    && cd "$GOPATH/src/github.com/rackn/provisioner-mgmt" \
    && /usr/local/go/bin/go build && cp provisioner-mgmt /usr/local/bin

