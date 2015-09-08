#FROM ubuntu:14.04
FROM compose_consul

RUN apt-get install -y ntp

COPY entrypoint.sh /sbin/entrypoint.sh
COPY ntp.json /etc/consul.d/ntp.json
RUN chmod 755 /sbin/entrypoint.sh

ENTRYPOINT ["/sbin/entrypoint.sh"]

# Ports to map
# EXPOSE 123
