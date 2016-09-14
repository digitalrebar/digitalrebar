#!/bin/bash

# cleanup
rm "consul_0.6.4_linux_amd64.*"
rm "consul"
# install consul
wget 'https://releases.hashicorp.com/consul/0.6.4/consul_0.6.4_linux_amd64.zip'
unzip "consul_0.6.4_linux_amd64.zip"
# check
./consul --version
