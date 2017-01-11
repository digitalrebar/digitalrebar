FROM gcr.io/google_containers/ubuntu-slim:0.5
MAINTAINER Victor Lowther <victor@rackn.com>

ARG DR_TAG

# Get packages
RUN mkdir -p /usr/local/sbin/ /usr/local/entrypoint.d /etc/rebar-data \
    && apt-get update \
    && apt-get -y dist-upgrade \
    && apt-get install -y bash curl ca-certificates unzip git jq build-essential wget iproute2 less vim --no-install-recommends \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

ADD http://localhost:28569/${DR_TAG}/linux/amd64/rebar /usr/local/bin/rebar
COPY docker-entrypoint.sh /sbin/docker-entrypoint.sh
RUN chmod 755 /usr/local/bin/rebar
