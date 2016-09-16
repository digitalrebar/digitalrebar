FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

ENV SERVICE_NAME provisioner
ENV TFTPROOT /tftpboot
ENV WEBPORT 8091
ENV APIPORT 8092

ARG DR_TAG=latest
ENV DR_TAG ${DR_TAG}

# Get packages
RUN mkdir -p linux/amd64 && \
    curl -fgL https://s3-us-west-2.amazonaws.com/rebar-bins/${DR_TAG}/sha256sums \
         -o rebar-bins.sha256sum && \
    for bin in provisioner-mgmt sign-it; do \
        curl -fgL "https://s3-us-west-2.amazonaws.com/rebar-bins/${DR_TAG}/linux/amd64/$bin" \
             -o "linux/amd64/$bin" && \
        (grep "linux/amd64/$bin" rebar-bins.sha256sum | sha256sum -c) && \
        mv "linux/amd64/$bin" "/usr/local/bin/$bin" && \
        chmod 755 "/usr/local/bin/$bin"; \
    done; \
    rm rebar-bins.sha256sum && \
    rm -rf linux

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

# Get Latest Go
RUN apt-get -y update && apt-get -y install bsdtar createrepo xz-utils unzip bsdmainutils
RUN apt-get -y purge make build-essential
RUN curl -fgL https://s3-us-west-2.amazonaws.com/rebar-sws/sws-linux-amd64 -o /usr/local/bin/sws && chmod 755 /usr/local/bin/sws
RUN curl -fgL -o /tmp/elilo.tar.gz http://downloads.sourceforge.net/project/elilo/elilo/elilo-3.16/elilo-3.16-all.tar.gz
RUN curl -fgL -o /tmp/syslinux-6.03.tar.xz https://www.kernel.org/pub/linux/utils/boot/syslinux/syslinux-6.03.tar.xz
# for esxi
RUN curl -fgL -o /tmp/syslinux-3.86.tar.xz https://www.kernel.org/pub/linux/utils/boot/syslinux/3.xx/syslinux-3.86.tar.xz
RUN curl -fgL -o /tmp/ipxe.efi http://boot.ipxe.org/ipxe.efi
RUN curl -fgL -o /tmp/ipxe.pxe http://boot.ipxe.org/ipxe.pxe
#RUN curl -fgL -o /tmp/busybox https://www.busybox.net/downloads/binaries/busybox-x86_64
RUN curl -fgL -o /tmp/busybox https://s3-us-west-2.amazonaws.com/rackn-busybox/busybox

COPY entrypoint.d/*.sh /usr/local/entrypoint.d/
COPY start-up.sh udhcpc_config stage1_init /tmp/
COPY explode_iso.sh /

COPY update-nodes/templates/ /opt/provisioner-mgmt/templates
COPY update-nodes/bootenvs/ /opt/provisioner-mgmt/bootenvs

