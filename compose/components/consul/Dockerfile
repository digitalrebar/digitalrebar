FROM ubuntu:14.04

RUN apt-get update && apt-get install -y curl unzip \
  && mkdir -p /tmp/consul /ui \
  && curl -fL -o consul.zip https://dl.bintray.com/mitchellh/consul/0.5.2_linux_amd64.zip \
  && curl -fL -o consul_ui.zip https://dl.bintray.com/mitchellh/consul/0.5.2_web_ui.zip \
  && unzip consul.zip -d /usr/local/bin \
  && unzip consul_ui.zip -d /ui \
  && rm consul.zip consul_ui.zip \
  && mkdir -p /etc/consul.d \
  && apt-get purge -y unzip

VOLUME /tmp/consul

ENTRYPOINT ["consul", "agent", "-config-dir=/etc/consul.d", "-data-dir=/tmp/consul", "-ui-dir=/ui/dist"]

# These are the ports we want exposed, but we don't want all to inherit
#EXPOSE 8300 8301 8301/udp 8302 8302/udp 8400 8500 8600 8600/udp
