#!/bin/bash

enable_flash=$(read_attribute 'rebar/hw_support/enable_flash')
if [[ $enable_flash == false ]]; then
    echo "Disabling flash because system-wide settings"
    exit 0
fi

skip_flash="$(get_attr skip-flash)"
if [[ $skip_flash = true ]]; then
    echo "Skipping all flash operations due to skip-flash being true"
    exit 0
fi

baseurl="$(get_attr dell-dsu-base-url)"
release="$(get_attr dell-dsu-block-release)"

if ! grep -q 'sledgehammer\.iso' /proc/cmdline; then
    echo "System not in Sledgehammer, exiting"
    exit 0
fi

# We don't want to worry about failing due to misconfigured other repos.
if [[ ! -d /etc/yum.repos.d.bak ]]; then
    mv /etc/yum.repos.d /etc/yum.repos.d.bak
    mkdir -p /etc/yum.repos.d
fi

REPO_NAME=dell-system-update
url="$baseurl"
if [[ $release ]]; then
    url="${url}/$release"
fi

cat > /etc/yum.repos.d/${REPO_NAME}.repo <<EOF
[${REPO_NAME}_independent]
name=${REPO_NAME}_independent
baseurl=${url}/os_independent/
gpgcheck=0
enabled=1
exclude=dell-system-update*.$exclude_arch

[${REPO_NAME}_dependent]
name=${REPO_NAME}_dependent
baseurl=${url}/os_dependent/RHEL7_64/
gpgcheck=0
enabled=1
EOF

yum -y install dell-system-update

need_reboot=no
failed=no

dsu --non-interactive --log-level=1 --output-log-file=/root/dsu.log || \
    case $? in
        1)
            # If the system is up to date, this is the return code and
            # the log will have the message "No Applicable Updates Available"
            if ! fgrep -q "No Applicable Updates Available" /root/dsu.log; then
                failed=yes
            fi;;
        8)
            echo "Reboot required to finish updates"
            need_reboot=yes;;
        25)
            echo "Partial ugrade failure"
            failed=yes;;
        26)
            echo "Partial upgrade failure, but system reboot required"
            need_reboot=yes
            failed=yes;;
        *)
            echo "Unhandleable exit code $?"
            echo "Lease refer to the DSU manual at http://www.dell.com/support/manuals/uk/en/ukdhs1/system-update-v1.4.0/DSU_UG_1.4/Updating-the-system-using-DSU";;
    esac
if [[ $failed = yes ]]; then
    echo "Log of failed DSU run:"
    cat /root/dsu.log
fi

if [[ $need_reboot = yes ]]; then
    echo "Reboot required, rebooting."
    reboot
fi

if [[ -d /etc/yum.repos.d.bak ]]; then
    rm -rf /etc/yum.repos.d
    mv /etc/yum.repos.d.bak /etc/yum.repos.d
fi

[[ $failed = no ]]
