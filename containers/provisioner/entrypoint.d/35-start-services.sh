#!/bin/bash

# Serve tftp and web
provisioner-mgmt \
    --api-port "$APIPORT" \
    --static-port "${WEBPORT}" \
    --tftp-port "$TFTPPORT" \
    --file-root "$TFTPROOT" &
