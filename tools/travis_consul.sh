#!/bin/bash

# cleanup
rm "0.4.1_linux_amd64.*"
rm "consul"
# install consul
wget 'https://dl.bintray.com/mitchellh/consul/0.4.1_linux_amd64.zip'
unzip "0.4.1_linux_amd64.zip"

