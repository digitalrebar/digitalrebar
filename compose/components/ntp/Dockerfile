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

RUN apt-get install -y ntp

COPY entrypoint.sh /sbin/entrypoint.sh
COPY ntp.json /etc/consul.d/ntp.json
RUN chmod 755 /sbin/entrypoint.sh

ENTRYPOINT ["/sbin/entrypoint.sh"]

# Ports to map
# EXPOSE 123
