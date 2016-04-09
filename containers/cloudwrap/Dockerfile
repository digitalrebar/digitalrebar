FROM digitalrebar/deploy-service-wrapper
MAINTAINER Victor Lowther <victor@rackn.com>
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]



RUN apt-get -y update && \
    apt-get -y install software-properties-common wget && \
    apt-add-repository ppa:brightbox/ruby-ng && \
    add-apt-repository "deb http://archive.ubuntu.com/ubuntu $(lsb_release -sc)-backports main restricted universe multiverse" && \
    apt-get -y update && \
    apt-get -y install ruby2.2 ruby2.2-dev make cmake curl build-essential jq libxml2-dev libcurl4-openssl-dev libssl-dev python2.7-dev python-pip && \
    gem install bundler && \
    mkdir -p /var/cache/cloudwrap/gems
RUN pip install python-openstackclient && \
	pip install positional && \
	pip install wrapt && \
	pip install prettytable && \
	pip install oslo.serialization && \
	pip install requestsexceptions && \
	pip install appdirs && \
	pip install pyyaml && \
	pip install simplejson && \
	pip install unicodecsv && \
	pip install netifaces && \
	pip install warlock

COPY cloudwrap /opt/cloudwrap

RUN (cd /opt/cloudwrap && bundle install --path /var/cache/cloudwrap/gems --standalone --binstubs /usr/local/bin)

COPY entrypoint.d/*.sh /usr/local/entrypoint.d/
