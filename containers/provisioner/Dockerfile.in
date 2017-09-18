FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

ENV SERVICE_NAME provisioner
ENV TFTPROOT /tftpboot
ENV WEBPORT 8091
ENV APIPORT 8092
ENV TFTPPORT 69

ARG DR_TAG
# Get packages
ADD http://localhost:28569/${DR_TAG}/linux/amd64/provisioner-mgmt /usr/local/bin/
ADD http://localhost:28569/${DR_TAG}/linux/amd64/sign-it /usr/local/bin/
ADD https://s3-us-west-2.amazonaws.com/rackn-busybox/busybox /tmp/busybox
ADD http://boot.ipxe.org/ipxe.efi /tmp/ipxe.efi
ADD http://boot.ipxe.org/ipxe.pxe /tmp/ipxe.pxe
ADD https://github.com/stedolan/jq/releases/download/jq-1.5/jq-linux64 /tmp/jq
RUN cd /tmp \
    && curl -fgL http://downloads.sourceforge.net/project/elilo/elilo/elilo-3.16/elilo-3.16-all.tar.gz -o /tmp/elilo.tar.gz \
    && curl -fgL https://s3.amazonaws.com/rackn-sledgehammer/provisioner/syslinux-6.03.tar.xz -o /tmp/syslinux-6.03.tar.xz \
    && curl -fgL https://s3.amazonaws.com/rackn-sledgehammer/provisioner/syslinux-3.86.tar.xz -o /tmp/syslinux-3.86.tar.xz \
    && curl -fgL https://git.ipxe.org/releases/wimboot/wimboot-2.5.2.tar.bz2 -o /tmp/wimboot-2.5.2.tar.bz2 \
    && curl -fgL https://rubygems.org/downloads/cstruct-1.0.1.gem -o /tmp/cstruct-1.0.1.gem
COPY entrypoint.d/*.sh /usr/local/entrypoint.d/
COPY start-up.sh udhcpc_config stage1_init /tmp/
COPY explode_iso.sh /

RUN mkdir -p /opt/provisioner-mgmt

RUN chmod 755 /usr/local/bin/*

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

RUN apt-get -y update \
    && apt-get -y install bsdtar createrepo xz-utils unzip bsdmainutils p7zip-full samba \
    && apt-get -y purge make build-essential

COPY smb.conf /etc/samba/smb.conf
