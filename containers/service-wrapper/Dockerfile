FROM digitalrebar/base
MAINTAINER Victor Lowther <victor@rackn.com>

COPY entrypoint.d/*.sh /usr/local/entrypoint.d/
RUN  apt-get -y update && \
     apt-get -y install jq && \
     curl -fgL -o /usr/local/bin/rebar \
         https://s3-us-west-2.amazonaws.com/rebar-cli/rebar-linux-amd64 && \
     chmod 755 /usr/local/bin/rebar