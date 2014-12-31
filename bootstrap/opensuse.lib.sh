#!/bin/bash
install_prereqs() {
    zypper -n update
    zypper -n install curl ruby20 ruby20-devel ruby20-devel-extra
    zypper -n install \
           http://opscode-omnibus-packages.s3.amazonaws.com/el/6/x86_64/chef-11.16.4-1.el6.x86_64.rpm
}
