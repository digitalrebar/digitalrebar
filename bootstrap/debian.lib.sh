#!/bin/bash
install_prereqs() {
    apt-get -y --force-yes update
    apt-get -y --force-yes upgrade
    apt-get -y --force-yes install curl
}
