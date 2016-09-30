#!/bin/bash

# Serve tftp and web
provisioner-mgmt \
    --api-port "$APIPORT" \
    --static-port "${WEBPORT}" \
    --tftp-port 69 \
    --file-root "$TFTPROOT" &
