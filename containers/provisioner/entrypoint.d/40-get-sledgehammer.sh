#!/bin/bash

PROV_TFTPROOT="/tftpboot"
PROV_WEBPORT="8091"
PROV_WEB="http://${EXTERNAL_IP%%/*}:$PROV_WEBPORT"

if [[ ! $PROV_SLEDGEHAMMER_SIG ]] ; then
  echo "Sledgehammer Hash not specified"
  exit 1
fi

if [[ ! $PROV_SLEDGEHAMMER_URL ]] ; then
  echo "Sledgehammer URL not specified"
  exit 1
fi

for d in files nodes discovery/pxelinux.cfg; do
    mkdir -p "$PROV_TFTPROOT/$d"
done
cp /usr/local/bin/rebar "$PROV_TFTPROOT/files/rebar"
cp /tmp/start-up.sh "$PROV_TFTPROOT/nodes/start-up.sh"

# Get sledgehammer
SS_URL=$PROV_SLEDGEHAMMER_URL/$PROV_SLEDGEHAMMER_SIG
SS_DIR=$PROV_TFTPROOT/sledgehammer/$PROV_SLEDGEHAMMER_SIG
mkdir -p "$SS_DIR"
if [[ ! -e $SS_DIR/sha1sums ]]; then
    for f in initrd0.img vmlinux0 sha1sums; do
        curl -fgL -o "$SS_DIR/$f" "$SS_URL/$f"
    done
  sha1sum -c sha1sums
fi

# Extract lpxelinux and elilo
(
    cd "$PROV_TFTPROOT/discovery"

    for f in syslinux-6.03/bios/com32/elflink/ldlinux/ldlinux.c32 \
                 syslinux-6.03/bios/core/lpxelinux.0; do
        tar xJf /tmp/syslinux.tar.xz "$f" -O >"${f##*/}"
    done
    tar xzf '/tmp/elilo.tar.gz' ./elilo-3.16-x86_64.efi
    tar xzf '/tmp/elilo.tar.gz' ./elilo-3.16-ia32.efi
    tar xzf '/tmp/elilo.tar.gz' ./elilo-3.16-ia64.efi
    mv elilo-3.16-x86_64.efi bootx64.efi
    mv elilo-3.16-ia32.efi bootia32.efi
    mv elilo-3.16-ia64.efi bootia64.efi
)

# Make it the discovery image
rm -f "$PROV_TFTPROOT/discovery/initrd0.img" "$PROV_TFTPROOT/discovery/vmlinuz0"
cp "$PROV_TFTPROOT/sledgehammer/$PROV_SLEDGEHAMMER_SIG/initrd0.img" \
   "$PROV_TFTPROOT/sledgehammer/$PROV_SLEDGEHAMMER_SIG/vmlinuz0" \
   "$PROV_TFTPROOT/discovery"

if which selinuxenabled && \
        selinuxenabled && \
        ! (ls -adZ "$PROV_TFTPROOT" |grep -q public_content_t); then
    semanage fcontext -a -f '' -t public_content_t "$PROV_TFTPROOT"
    semanage fcontext -a -f '' -t public_content_t "${PROV_TFTPROOT}(/.*)?"
fi

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
             "initrd=initrd0.img"
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
             > "$PROV_TFTPROOT/discovery/pxelinux.cfg/default"

elilo_cfg "discovery" \
          "vmlinux0" \
          "${SLEDGE_ARGS[*]}" \
          "initrd0.img" \
          > "$PROV_TFTPROOT/discovery/elilo.conf"

