#!/bin/bash

PROV_WEB="http://${EXTERNAL_IP%%/*}:${WEBPORT}"
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
             "rebar.web=${EXTERNAL_REBAR_ENDPOINT}"
             "rebar.state=discovery"
             "rebar.install.key=${REBAR_KEY}"
            )

pxelinux_cfg "discovery" \
             "$PROV_WEB/discovery/vmlinuz0" \
             "${SLEDGE_ARGS[*]}" \
             "$PROV_WEB/discovery/initrd0.img" \
             > "${TFTPROOT}/discovery/pxelinux.cfg/default"

elilo_cfg "discovery" \
          "vmlinux0" \
          "${SLEDGE_ARGS[*]}" \
          "initrd0.img" \
          > "${TFTPROOT}/discovery/elilo.conf"
