#!/bin/bash
install_prereqs() {
    yum -y upgrade
    yum -y install ruby ruby-devel which curl
    yum -y install http://opscode-omnibus-packages.s3.amazonaws.com/el/6/x86_64/chef-11.16.4-1.el6.x86_64.rpm
}
