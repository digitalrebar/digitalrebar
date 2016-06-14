FROM library/alpine:latest
MAINTAINER Victor Lowther <victor@rackn.com>

ENV GOPATH /usr/local

RUN apk update && apk upgrade
RUN apk add bash bash-completion curl jq git go openssh
RUN go get github.com/digitalrebar/rebar-api/rebar

# Add Chef

RUN apk add ruby ruby-json ruby-dev gcc make ruby-dev musl-dev libffi libffi-dev
RUN gem install chef --no-ri --no-rdoc

COPY start-up.sh /usr/local/bin/start-up
COPY sshd_config /etc/ssh/sshd_config

CMD /usr/local/bin/start-up