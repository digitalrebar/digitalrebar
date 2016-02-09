#!/bin/bash
if [[ ! $PROV_SLEDGEHAMMER_SIG ]] ; then
  echo "Sledgehammer Hash not specified"
  exit 1
fi

if [[ ! $PROV_SLEDGEHAMMER_URL ]] ; then
  echo "Sledgehammer URL not specified"
  exit 1
fi

for d in files nodes discovery/pxelinux.cfg; do
    mkdir -p "${TFTPROOT}/$d"
done
cp /usr/local/bin/rebar "${TFTPROOT}/files/rebar"
cp /tmp/start-up.sh "${TFTPROOT}/nodes/start-up.sh"

# Get sledgehammer
SS_URL=$PROV_SLEDGEHAMMER_URL/$PROV_SLEDGEHAMMER_SIG
SS_DIR=${TFTPROOT}/sledgehammer/$PROV_SLEDGEHAMMER_SIG
mkdir -p "$SS_DIR"
if [[ ! -e $SS_DIR/sha1sums ]]; then
    for f in initrd0.img vmlinuz0 sha1sums; do
        curl -fgL -o "$SS_DIR/$f" "$SS_URL/$f"
    done
    if ! (cd "$SS_DIR" && sha1sum -c sha1sums); then
        echo "Download of sledgehammer failed or is corrupt!"
        rm -f "$SS_DIR/sha1sums"
        exit 1
    fi
fi

# Extract lpxelinux and elilo
(
    cd "${TFTPROOT}/discovery"

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
rm -f "${TFTPROOT}/discovery/initrd0.img" "${TFTPROOT}/discovery/vmlinuz0"
cp "${TFTPROOT}/sledgehammer/$PROV_SLEDGEHAMMER_SIG/initrd0.img" \
   "${TFTPROOT}/sledgehammer/$PROV_SLEDGEHAMMER_SIG/vmlinuz0" \
   "${TFTPROOT}/discovery"

if which selinuxenabled && \
        selinuxenabled && \
        ! (ls -adZ "${TFTPROOT}" |grep -q public_content_t); then
    semanage fcontext -a -f '' -t public_content_t "${TFTPROOT}"
    semanage fcontext -a -f '' -t public_content_t "${TFTPROOT}(/.*)?"
fi
