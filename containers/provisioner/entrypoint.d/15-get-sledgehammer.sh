#!/bin/bash
if [[ ! $PROV_SLEDGEHAMMER_SIG ]] ; then
  echo "Sledgehammer Hash not specified"
  exit 1
fi

if [[ ! $PROV_SLEDGEHAMMER_URL ]] ; then
  echo "Sledgehammer URL not specified"
  exit 1
fi

for d in files machines pxelinux.cfg; do
    mkdir -p "${TFTPROOT}/$d"
done

# Backwards compatibility with older sledgehammer
if ! [[ -L $TFTPROOT/nodes ]]; then
    (cd "$TFTPROOT"; rm -rf nodes && ln -s machines nodes)
fi
cp "$(which rebar)" "${TFTPROOT}/files/rebar"
cp /tmp/start-up.sh "${TFTPROOT}/nodes/start-up.sh"
cp /tmp/ipxe.* "${TFTPROOT}"

# Get sledgehammer
SS_URL=$PROV_SLEDGEHAMMER_URL/$PROV_SLEDGEHAMMER_SIG
SS_DIR=${TFTPROOT}/sledgehammer/$PROV_SLEDGEHAMMER_SIG
mkdir -p "$SS_DIR"
if [[ ! -e $SS_DIR/sha1sums ]]; then
    (with_local_proxy; curl -fgL -o "$SS_DIR/sha1sums" "$SS_URL/sha1sums")
    while read f; do
        (with_local_proxy; curl -fgL -o "$SS_DIR/$f" "$SS_URL/$f")
    done < <(awk '{print $2}' <"$SS_DIR/sha1sums")
    if ! (cd "$SS_DIR" && sha1sum -c sha1sums); then
        echo "Download of sledgehammer failed or is corrupt!"
        rm -f "$SS_DIR/sha1sums"
        exit 1
    fi
fi

# Make the second stage loader if we have not already done so
if [[ ! -f $SS_DIR/stage2.img ]]; then
    stage1_tmpdir="$TFTPROOT/.tmp/stage1"
    rm -rf "$stage1_tmpdir" || :
    mkdir -p "$stage1_tmpdir"

    # Little helper for running commands in the right location
    in_tmp() (
        cd "$stage1_tmpdir" && \
        "$@"
    )

    # General purpose decompressor.
    decompress() {
        # $1 = file to look at
        local -A formats readers
        formats['cat']='070701'
        formats['xzcat']=$'\xfd7zXZ'
        formats['lzop -d']=$'\x89LZO'
        formats['zcat']='8b1f'
        formats['lz4cat']='184d2204'
        formats['lz4cat -l']='184c2102'
        formats['bzcat']='BZh'
        formats['lzcat']='5d'
        readers['cat']='hexdump -n 6 -e "%c"'
        readers['xzcat']='hexdump -n 6 -e "%c"'
        readers['lzop -d']='hexdump -n 4 -e "%c"'
        readers['zcat']='hexdump -n 2 -e "%x"'
        readers['lz4cat']='hexdump -n 4 -e "%x"'
        readers['lz4cat -l']='hexdump -n 4 -e "%x"'
        readers['bzcat']='hexdump -n 3 -e "%c"'
        readers['lzcat']='hexdump -n 3 -e "%x"'
        local cmd= try=
        for try in "${!formats[@]}"; do
            [[ $(${readers["$try"]} "$1") = ${formats[$try]} ]] || continue
            cmd="$try"
            break
        done
        if [[ ! $cmd ]]; then
            echo "Failed to find decompressor for $1" >&2
            return 1
        fi
        $cmd < "$1"
    }
    
    # Utility for cutting an initramfs into 2 pieces
    cut_initramfs() {
        # $1 = directory to place segments
        # $2 = file to cut
        # $3 = index to start at
        # $4 = cut string to use
        local idx=${3:-0}
        decompress "$2">"$1/scratch.img" || {
            rm "$1/scratch.img"; return 1
        }
        local sep=$(grep -m 1 -P -o -a -b "$4" "$1/scratch.img" |tr '\0' '-')
        local frag=${sep#*:}
        local startnext=$((${sep%%:*} + ${#frag}))
        local endfirst=$((${sep%%:*} + 10))
        dd if="$1/scratch.img" count="$endfirst" iflag=count_bytes > "$1/ss$idx.img"
        idx=$((idx + 1))
        dd if="$1/scratch.img" skip="$startnext" iflag=skip_bytes > "$1/ss$idx.img"
        rm "$1/scratch.img"
    }
    in_tmp mkdir -p ss0/dev ss0/proc ss0/sys ss0/bin ss0/etc \
           ss0/usr/share/udhcpc ss0/tmp ss0/newinitramfs \
           ss1
    in_tmp cut_initramfs "." "$SS_DIR/initrd0.img" 0 'TRAILER!!!\x0*'
    if [[ ! -f $stage1_tmpdir/ss1.img ]]; then
        echo "Sledgehammer initrd did not have at least 2 parts"
        exit 1
    fi

    # There are more than one initramfs images packed together.
    # The first one usually contains the early_cpio stuff
    if ! in_tmp grep -q 'early_cpio' ss0.img; then
        echo "First initramfs missing early_cpio!"
        exit 1
    fi

    echo "Extracting early_cpio from initramfs"
    (cd "$stage1_tmpdir/ss0"; cpio -id --no-absolute-filenames < ../ss0.img) || :
    in_tmp rm ss0.img
    echo "Extracting remaining original initramfs layers"
    (
        cd "$stage1_tmpdir/ss1"
        zcat ../ss1.img | while cpio -id --no-absolute-filenames; do : ; done
    )
    echo "Extracting kernel modules from second stage initramfs"
    (
        cd "$stage1_tmpdir/ss0"
        mkdir lib
        cp -a "$stage1_tmpdir/ss1/lib/modules" "$stage1_tmpdir/ss1/lib/firmware" lib
        # Clean out stuff we don't want to have or care about for the stage1 initramfs
        (cd lib/modules/*/kernel/drivers; rm -rf ata md usb dca firewire scsi mmc cdrom block gpu)
        (cd lib/firmware; rm -rf radeon)
        echo "Compressing kernel modules"
        find lib |sort |cpio -o -R 0:0 --format=newc |xz -T0 -c >lib.cpio.xz
        rm -rf lib
        echo "Installing busybox"
        mv /tmp/busybox bin/busybox
        (cd bin && chmod 755 busybox && ./busybox --install .)
        cp /tmp/stage1_init init
        chmod 755 init
        cp /tmp/udhcpc_config usr/share/udhcpc/default.script
        chmod 755 usr/share/udhcpc/default.script
        echo "Creating first-stage initramfs"
        find | sort |cpio -o -R 0:0 --format=newc |gzip -9 >"$SS_DIR/stage1.img"
    )
    echo "Flattening remaining original initramfs layers"
    (
        cd "$stage1_tmpdir/ss1"
        find |sort |cpio -o -R 0:0 --format=newc |xz -1 -T0 -c >"$SS_DIR/stage2.img"
    )
    in_tmp rm -rf ss1
    rm -rf "$stage1_tmpdir"
fi

# Lift everything out of the discovery directory and replace it with a symlink
# Symlink is for backwards compatibility
if [[ -d $TFTPROOT/discovery && ! -L $TFTPROOT/discovery ]]; then
    for f in "${TFTPROOT}/discovery/"*; do
        [[ -e $f ]] || continue
        mv "$f" "${TFTPROOT}/${f##*/}"
    done
    rmdir "$TFTPROOT/discovery"
fi
[[ -L $TFTPROOT/discovery ]] || (cd "${TFTPROOT}"; ln -sf . discovery)

# Extract lpxelinux and elilo
(
    cd "${TFTPROOT}"
    
    for f in syslinux-6.03/bios/com32/elflink/ldlinux/ldlinux.c32 \
                 syslinux-6.03/bios/core/lpxelinux.0 \
                 syslinux-6.03/bios/com32/modules/pxechn.c32 \
                 syslinux-6.03/bios/com32/libutil/libutil.c32; do
        tar xJf /tmp/syslinux-6.03.tar.xz "$f" -O >"${f##*/}"
    done
    tar xJf /tmp/syslinux-3.86.tar.xz syslinux-3.86/core/pxelinux.0 -O > esxi.0
    tar xzf '/tmp/elilo.tar.gz' ./elilo-3.16-x86_64.efi
    tar xzf '/tmp/elilo.tar.gz' ./elilo-3.16-ia32.efi
    tar xzf '/tmp/elilo.tar.gz' ./elilo-3.16-ia64.efi
    mv elilo-3.16-x86_64.efi bootx64.efi
    mv elilo-3.16-ia32.efi bootia32.efi
    mv elilo-3.16-ia64.efi bootia64.efi
)

# Make it the discovery image

(cd "$TFTPROOT"; rm initrd0.img stage*.img vmlinuz0) || :
cp "$SS_DIR/"stage*.img "$SS_DIR/vmlinuz0" "$TFTPROOT"

if which selinuxenabled && \
        selinuxenabled && \
        ! (ls -adZ "${TFTPROOT}" |grep -q public_content_t); then
    semanage fcontext -a -f '' -t public_content_t "${TFTPROOT}"
    semanage fcontext -a -f '' -t public_content_t "${TFTPROOT}(/.*)?"
fi
