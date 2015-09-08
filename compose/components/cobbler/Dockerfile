FROM compose_consul

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

