#!/bin/bash

if [[ $FORWARDER_IP ]]; then
    cp /root/internal-provisioner-service.json /etc/consul.d/provisioner.json
    cp /root/internal-tftp-service.json /etc/consul.d/tftp.json
else
    sed -e "s/FILLMEIN/${EXTERNAL_IP%%/*}/" root/external-provisioner-service.json >/etc/consul.d/provisioner.json
    sed -e "s/FILLMEIN/${EXTERNAL_IP%%/*}/" root/external-tftp-service.json >/etc/consul.d/tftp.json
fi
consul reload

# Start tftpd
/usr/sbin/in.tftpd --listen --user root --address 0.0.0.0:69 --secure /tftpboot &
# Start web server
/usr/local/bin/sws --listen=:$PROV_WEBPORT --site=$PROV_TFTPROOT </dev/null 2>&1 1>/var/log/sws.log &
