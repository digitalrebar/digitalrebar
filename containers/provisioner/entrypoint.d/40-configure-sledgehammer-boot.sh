#!/bin/bash

PROV_IP=${EXTERNAL_IP%%/*}
PROV_WEB="http://${PROV_IP}:${WEBPORT}"
# Make a pxelinux config file on stdout.
pxelinux_cfg() {
    # $1 = name of the boot option
    # $2 = Path to the kernel to boot
    # $3 = Kernel arguments to pass
    # $4 = Path to the initrd, if any
    local append_line=$3
    if [[ $4 ]]; then
        append_line="initrd=$4 $append_line"
    fi
cat <<EOF
DEFAULT $1
PROMPT 0
TIMEOUT 10
LABEL $1
  KERNEL $2
  APPEND $append_line
  IPAPPEND 2
EOF
}

# Make an elilo config file on stdout
elilo_cfg() {
    # $1 = name of the boot option
    # $2 = Path to the kernel to boot
    # $3 = Kernel arguments to pass
    # $4 = Path to the initrd, if any
    cat <<EOF
delay=1
timeout=20
verbose=5
image=$2
label=$1
append="$3"
EOF
    if [[ $4 ]]; then
        echo "initrd=$4"
    fi
}

SLEDGE_ARGS=("rootflags=loop"
             "root=live:/sledgehammer.iso"
             "rootfstype=auto"
             "ro"
             "liveimg"
             "rd_NO_LUKS"
             "rd_NO_MD"
             "rd_NO_DM"
             "provisioner.web=$PROV_WEB"
             "stage2=$PROV_WEB/discovery/stage2.img"
             "rebar.web=${EXTERNAL_REBAR_ENDPOINT}"
             "rebar.install.key=${REBAR_KEY}"
            )
cat > "$TFTPROOT/default.ipxe" <<EOF
#!ipxe
chain tftp://$PROV_IP/discovery/\${netX/ip}.ipxe && goto bail || goto sledgehammer
:sledgehammer
kernel tftp://$PROV_IP/discovery/vmlinuz0 ${SLEDGE_ARGS[@]} BOOTIF=01-\${netX/mac:hexhyp}
initrd tftp://$PROV_IP/discovery/stage1.img
boot
:bail
exit
EOF

pxelinux_cfg "discovery" \
             "/discovery/vmlinuz0" \
             "${SLEDGE_ARGS[*]}" \
             "/discovery/stage1.img" \
             > "${TFTPROOT}/discovery/pxelinux.cfg/default"

elilo_cfg "discovery" \
          "/discovery/vmlinuz0" \
          "${SLEDGE_ARGS[*]}" \
          "/discovery/stage1.img" \
          > "${TFTPROOT}/discovery/elilo.conf"
