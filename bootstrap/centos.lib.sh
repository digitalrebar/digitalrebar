#!/bin/bash
install_prereqs() {
    sed -i -e '/enabled/ s/0/1/' /etc/yum.repos.d/CentOS-Base.repo
    yum -y makecache
    if locale 2>&1 |grep -q Cannot; then
        yum -y reinstall glibc-common
        .  <(locale)
    fi
    yum -y install curl which
    yum -y install http://opscode-omnibus-packages.s3.amazonaws.com/el/6/x86_64/chef-11.16.4-1.el6.x86_64.rpm
}
