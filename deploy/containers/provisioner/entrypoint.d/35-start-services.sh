#!/bin/bash

# Serve tftp and web
/usr/local/bin/sws --listen=:${WEBPORT} --tftp=:69 --site=${TFTPROOT} </dev/null 2>&1 &
/usr/local/bin/provisioner-mgmt \
    --api-port "$APIPORT" \
    --file-root "$TFTPROOT" \
    --host "provisioner,provisioner-mgmt,provisioner-mgmt-service,$IP,${EXTERNAL_IP%%/*},${HOSTNAME},127.0.0.1,localhost" \
    --provisioner "http://${EXTERNAL_IP%%/*}:$WEBPORT" \
    --command "$EXTERNAL_REBAR_ENDPOINT" &
