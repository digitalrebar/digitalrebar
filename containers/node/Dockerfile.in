FROM gcr.io/google_containers/ubuntu-slim:0.5

ARG DR_TAG

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]

# Get packages
RUN apt-get update \
  && apt-get -y --no-install-recommends install git jq openssh-server curl ca-certificates \
  && curl -fgL -o '/tmp/chef.deb' 'https://packages.chef.io/stable/ubuntu/12.04/chef_12.16.42-1_amd64.deb' \
  && dpkg -i /tmp/chef.deb \
  && rm -f /tmp/chef.deb
ADD http://localhost:28569/${DR_TAG}/linux/amd64/rebar /usr/local/bin/rebar
COPY docker-entrypoint.sh /sbin/docker-entrypoint.sh
RUN chmod 755 /usr/local/bin/rebar

COPY docker-entrypoint.sh /sbin/docker-entrypoint.sh

