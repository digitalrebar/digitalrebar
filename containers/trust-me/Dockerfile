FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

ENV SERVICE_NAME trust-me

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]
RUN for i in trust-me sign-it; do \
    curl -fgL -o "/usr/local/bin/$i" \
         "https://s3-us-west-2.amazonaws.com/rebar-trust-me/$i" && \
    chmod 755 "/usr/local/bin/$i" \
    done

COPY test_tm_up.sh /usr/local/bin/test_tm_up.sh
RUN chmod 755 /usr/local/bin/test_tm_up.sh
COPY entrypoint.d/*.sh /usr/local/entrypoint.d/

