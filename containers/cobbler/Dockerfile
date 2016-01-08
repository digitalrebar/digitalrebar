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

RUN apt-get update \
  && apt-get install -y cobbler cobbler-web \
  && apt-get install -y curl \
  && apt-get install -y debmirror \
  && mkdir -p /etc/consul.d

ADD cobbler-start /usr/local/sbin/
COPY provisioner.json /etc/consul.d/provisioner.json
COPY provisioner-mgmt.json /etc/consul.d/provisioner-mgmt.json

ENTRYPOINT ["/usr/local/sbin/cobbler-start"]

EXPOSE 25151
EXPOSE 69
EXPOSE 80
EXPOSE 443

