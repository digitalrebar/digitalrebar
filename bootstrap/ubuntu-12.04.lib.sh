#!/bin/bash
locale-gen en_US.UTF-8
update-locale LANG=en_US.UTF-8
install_prereqs() {
    apt-get -y --force-yes update
    apt-get -y --force-yes dist-upgrade
    apt-get -y --force-yes install build-essential curl
}
