#!/bin/bash

# Serve tftp and web
/usr/local/bin/sws --listen=:${WEBPORT} --tftp=:69 --site=${TFTPROOT} </dev/null 2>&1 &
/usr/local/bin/provisioner-mgmt \
    --api-port "$APIPORT" \
    --file-root "$TFTPROOT" \
    --cacert /etc/prov-base-cert.pem \
    --cert /etc/prov-cert.pem \
    --key /etc/prov-key.pem \
    --provisioner "http://${EXTERNAL_IP%%/*}:$WEBPORT" \
    --command "$EXTERNAL_REBAR_ENDPOINT" &
