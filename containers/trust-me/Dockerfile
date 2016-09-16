FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>

ENV SERVICE_NAME trust-me

ARG DR_TAG=latest
ENV DR_TAG ${DR_TAG}

# Set our command
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]
RUN mkdir -p linux/amd64 && \
    curl -fgL https://s3-us-west-2.amazonaws.com/rebar-bins/${DR_TAG}/sha256sums \
         -o rebar-bins.sha256sum
RUN for i in trust-me sign-it; do \
    curl -fgL https://s3-us-west-2.amazonaws.com/rebar-bins/${DR_TAG}/linux/amd64/$i \
         -o linux/amd64/$i && \
    (grep linux/amd64/$i rebar-bins.sha256sum | sha256sum -c) && \
    mv linux/amd64/$i /usr/local/bin/$i && \
    chmod 755 /usr/local/bin/$i ; \
    done
RUN rm -rf linux && rm -f rebar-bins.sha256sum

COPY test_tm_up.sh /usr/local/bin/test_tm_up.sh
RUN chmod 755 /usr/local/bin/test_tm_up.sh
COPY entrypoint.d/*.sh /usr/local/entrypoint.d/

