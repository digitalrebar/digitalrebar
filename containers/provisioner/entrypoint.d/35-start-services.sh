#!/bin/bash

PROV_IP=${EXTERNAL_IP%%/*}

# Serve tftp and web
provisioner-mgmt \
    --api-port "$APIPORT" \
    --static-ip "$PROV_IP" \
    --static-port "${WEBPORT}" \
    --tftp-port "$TFTPPORT" \
    --file-root "$TFTPROOT" &
