#!/bin/bash
archive_path=/opt/tools/sum_1.6.0_Linux_x86_64_20160128.tar.gz
bin_path=/opt/tools/bin/sum
if [[ -x $bin_path ]]; then
    echo "Supermicro Update Manager already installed"
elif ! [[ -f $archive_path ]]; then
    echo "Missing $archive_path"
    echo "Will not try to install Supermicro Update Manager"
else
    tmpdir=$(mktemp -d "/tmp/sum-XXXXX")
    (
        cd "$tmpdir"
        tar xzf "$archive_path"
        mkdir -p /opt/tools/bin
        cp */sum /opt/tools/bin
    )
    rm -rf $tmpdir
    if ! [[ -x $bin_path ]]; then
        echo "Failed to extract supermicro update manager binary"
        sleep 300
        exit 1
    fi
fi
