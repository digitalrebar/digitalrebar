#!/bin/bash

[[ -x /etc/init.d/crowbar_join.sh || \
    -x /etc/init.d/crowbar || \
    -x /usr/sbin/crowbar ]] && exit 0

set -x
set +e
webserver_re='"url"=>"([^"]+)"'
if ! [[ $(read_attribute "crowbar/provisioner/server/webservers") =~ $webserver_re ]]; then
    echo "Cannot figure out the URL to poll to see if we are ready to reboot!"
    exit 1
fi
webserver="${BASH_REMATCH[1]}"

# Nuke it all.
declare vg pv maj min blocks name
# Make sure that the kernel knows about all the partitions
for bd in /sys/block/sd*; do
    [[ -b /dev/${bd##*/} ]] || continue
    partprobe "/dev/${bd##*/}" || :
done
# Zap any volume groups that may be lying around.
vgscan --ignorelockingfailure -P
while read vg; do
    vgremove -f "$vg"
done < <(vgs --noheadings -o vg_name)
# Wipe out any LVM metadata that the kernel may have detected.
pvscan --ignorelockingfailure
while read pv; do
    pvremove -f -y "$pv"
done < <(pvs --noheadings -o pv_name)
# Now zap any partitions along with any RAID metadata that may exist.
while read maj min blocks name; do
    [[ -b /dev/$name && -w /dev/$name && $name != name ]] || continue
    [[ $name = loop* ]] && continue
    [[ $name = dm* ]] && continue
    mdadm --misc --zero-superblock --force /dev/$name
    if (( blocks >= 2048)); then
        dd "if=/dev/zero" "of=/dev/$name" "bs=512" "count=2048"
        dd "if=/dev/zero" "of=/dev/$name" "bs=512" "count=2048" "seek=$(($blocks - 2048))"
    else
        dd "if=/dev/zero" "of=/dev/$name" "bs=512" "count=$blocks"
    fi
done < <(tac /proc/partitions)


while true; do
    curl -s -f -L -o /tmp/bootstate "$webserver/nodes/$HOSTNAME/bootstate" && \
        [[ -f /tmp/bootstate && $(cat /tmp/bootstate) = *-install ]] && break
    sleep 1
done

