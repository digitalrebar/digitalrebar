FROM library/alpine:latest
MAINTAINER Victor Lowther <victor@rackn.com>

ENV GOPATH /usr/local

ARG DR_TAG=latest
ENV DR_TAG ${DR_TAG}

RUN apk update && apk upgrade
RUN apk add bash bash-completion curl jq git go openssh

RUN mkdir -p linux/amd64 && \
    curl -fgL https://s3-us-west-2.amazonaws.com/rebar-bins/${DR_TAG}/linux/amd64/rebar \
         -o linux/amd64/rebar && \
    curl -fgL https://s3-us-west-2.amazonaws.com/rebar-bins/${DR_TAG}/sha256sums \
         -o rebar-bins.sha256sum && \
    (grep "linux/amd64/rebar\$" rebar-bins.sha256sum | sha256sum -c && rm rebar-bins.sha256sum) && \
    mv linux/amd64/rebar /usr/local/bin/rebar && \
    chmod 755 /usr/local/bin/rebar && \
    rm -rf linux

# Add Chef

RUN apk add ruby ruby-json ruby-dev gcc make ruby-dev musl-dev libffi libffi-dev
RUN gem install chef --no-ri --no-rdoc

COPY start-up.sh /usr/local/bin/start-up
COPY sshd_config /etc/ssh/sshd_config

CMD /usr/local/bin/start-up
