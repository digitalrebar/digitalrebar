#!/bin/bash
#
# Build a sledgehammer image for Rebar and put it in the build cache.

# Copyright 2011, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# Author: VictorLowther

# We always use the C language and locale
export LANG="C"
export LC_ALL="C"
export PS4='${BASH_SOURCE}@${LINENO}(${FUNCNAME[0]}): '

GEM_RE='([^0-9].*)-([0-9].*)'
set -e

readonly currdir="$PWD"
export PATH="$PATH:/sbin:/usr/sbin:/usr/local/sbin"

# Location for caches that should not be erased between runs
[[ $CACHE_DIR ]] || CACHE_DIR="$HOME/.cache/digitalrebar/sledgehammer"
[[ $SLEDGEHAMMER_PXE_DIR ]] || SLEDGEHAMMER_PXE_DIR="$HOME/.cache/digitalrebar/tftpboot/discovery"
[[ $SLEDGEHAMMER_ARCHIVE ]] || SLEDGEHAMMER_ARCHIVE="$HOME/.cache/digitalrebar/tftpboot/sledgehammer"
[[ $CHROOT ]] || CHROOT="$CACHE_DIR/chroot"
[[ $SLEDGEHAMMER_LIVECD_CACHE ]] || SLEDGEHAMMER_LIVECD_CACHE="$CACHE_DIR/livecd_cache"
[[ $SYSTEM_TFTPBOOT_DIR ]] || SYSTEM_TFTPBOOT_DIR="/mnt/tftpboot"

REBAR_DIR="${0%/*}/.."

signature=$(sha1sum < <(cat "$0" "$REBAR_DIR/sledgehammer/"*) |awk '{print $1}')
SLEDGEHAMMER_IMAGE_DIR="$SLEDGEHAMMER_ARCHIVE/$signature"

cleanup() {
    set +e
    # Make sure that the loopback kernel module is loaded.
    [[ -e /dev/loop0 ]] || sudo modprobe loop

    while read line; do
        sudo losetup -d "${line%%:*}"
    done < <(sudo losetup -a |grep sledgehammer.iso)

    while read dev fs type opts rest; do
        sudo umount -d -l "$fs"
    done < <(tac /proc/self/mounts |grep -e "sledgehammer/chroot")
    sudo rm -rf --one-file-system "$CHROOT"
}

cleanup

mkdir -p "$CACHE_DIR" "$CHROOT" "$SLEDGEHAMMER_PXE_DIR" \
    "$SLEDGEHAMMER_IMAGE_DIR" "$SLEDGEHAMMER_LIVECD_CACHE"

if ! which cpio &>/dev/null; then
    die "Cannot find cpio, we cannot proceed."
fi

if ! which rpm rpm2cpio &>/dev/null; then
    die "Cannot find rpm and rpm2cpio, we cannot proceed."
fi

OS_BASIC_PACKAGES=(
    basesystem
    filesystem
    audit-libs
    bash
    binutils
    bzip2-libs
    centos-release
    chkconfig
    coreutils
    cracklib
    cracklib-dicts
    crontabs
    curl
    cyrus-sasl-lib
    dbus-libs
    device-mapper
    e2fsprogs
    e2fsprogs-libs
    elfutils-libelf
    ethtool
    expat
    file-libs
    findutils
    gawk
    gdbm
    glib2
    glibc
    glibc-common
    grep
    info
    initscripts
    iputils
    keyutils-libs
    krb5-libs
    libacl
    libattr
    libcap
    libcom_err
    libcurl
    libdb
    libffi
    libgcc
    libgcrypt
    libidn
    libselinux
    libsepol
    libssh2
    libstdc++
    libsysfs
    libuser
    libutempter
    libxml2
    libxml2-python
    logrotate
    lua
    m2crypto
    mcstrans
    mlocate
    ncurses
    ncurses-libs
    neon
    net-tools
    nspr
    nss
    nss-softokn
    nss-softokn-freebl
    nss-sysinit
    nss-util
    openldap
    openssl-libs
    pam
    passwd
    pcre
    popt
    psmisc
    python
    python-iniparse
    python-libs
    python-pycurl
    python-urlgrabber
    readline
    rpm
    rpm-libs
    rpm-python
    rsyslog
    sed
    setup
    shadow-utils
    sqlite
    tzdata
    xz
    xz-libs
    yum
    yum-metadata-parser
    yum-utils
    zlib
)

EXTRA_REPOS=('http://mirror.centos.org/centos/7/os/x86_64' \
    'http://mirror.centos.org/centos/7/updates/x86_64' \
    'http://mirror.centos.org/centos/7/extras/x86_64')

if [[ $http_proxy ]]; then
    export USE_PROXY=1
    raw_proxy="${http_proxy#*://}"
    raw_proxy="${raw_proxy%/}"
    proxy_re='^(.+):([0-9]+)$'
    hostsplit_re='(.*)@(.*)'
    userpass_re='(.*):(.*)'
    if [[ $raw_proxy =~ $proxy_re ]]; then
        export PROXY_PORT="${BASH_REMATCH[2]}"
        raw_proxy="${BASH_REMATCH[1]}"
    fi
    if [[ $raw_proxy =~ $hostsplit_re ]]; then
        raw_proxy="${BASH_REMATCH[2]}"
        export PROXY_USER="${BASH_REMATCH[1]}"
        if [[ ${BASH_REMATCH[1]} =~ $userpass_re ]]; then
            export PROXY_PASSWORD="${BASH_REMATCH[2]}"
            export PROXY_USER="${BASH_REMATCH[1]}"
        fi
    fi
    export PROXY_HOST="$raw_proxy"
    [[ $no_proxy ]] || no_proxy="localhost,localhost.localdomain,127.0.0.0/8,$PROXY_HOST"
else
    unset USE_PROXY PROXY_HOST PROXY_PORT PROXY_USER PROXY_PASS
fi

die() {
    printf "%s\n" "$@" >&2
    exit 1
}

debug() {
    printf "%s\n" "$@" >&2
}

# Run a command in our chroot environment.
in_chroot() {
    sudo -H chroot "$CHROOT" \
        /bin/bash -l -c "export PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin; $*"
}

# Install some packages in the chroot environment.
chroot_install() {
    in_chroot /usr/bin/yum clean metadata
    if [[ $1 ]]; then
        in_chroot /usr/bin/yum -y install "$@"
    fi
    in_chroot /usr/bin/yum -y update
}

trap cleanup EXIT INT QUIT TERM 0

# Make a repository file in the chroot environment.  We use this when we trap cleanup EXIT INT QUIT TERMget a URL
# from one of the packages files (as opposed to an RPM that contains repo info.
make_repo_file() {
    # $1 = name of repo
    # $2 = Priority
    # $3 = URL
    [[ -f "$CHROOT/etc/yum.repos.d/repo-$1.repo" ]] && return
    local repo=$(mktemp "/tmp/repo-$1-XXXX.repo")
    cat >"$repo" <<EOF
[$1]
name=Repo for $1
baseurl=$3
enabled=1
gpgcheck=0
EOF
    if [[ $RPM_PRIORITIES ]]; then
        echo "priority=$2" >>"$repo"
    fi
    sudo cp "$repo" "$CHROOT/etc/yum.repos.d/repo-$1.repo"
    rm "$repo"
}

# Add repositories to the chroot environment.
# This also add proxy information if needed.
add_repos() {
    for repo in "$@"; do
        rtype="${repo%% *}"
        rdest="${repo#* }"
        case $rtype in
            rpm) rdest="${rdest#* }"
                f="$(mktemp /tmp/tmp-XXXXXX.rpm)"
                curl -L -o "$f" "$rdest"
                sudo cp "$f" "$CHROOT/tmp"
                rm "$f"
                in_chroot /bin/rpm -Uvh "$f";;
            bare) make_repo_file $rdest;;
        esac
    done
    [[ $USE_PROXY = "1" ]] || return 0
    (   cd "$CHROOT"
        for f in etc/yum.repos.d/*; do
            [[ -f "$f" ]] || continue
            in_chroot /bin/grep -q "'^proxy='" "/$f" && continue
            in_chroot /bin/grep -q "'^baseurl=http://.*127\.0\.0\.1.*'" "/$f" && \
                continue
            in_chroot sed -i "'/^name/ a\proxy=http://$PROXY_HOST:$PROXY_PORT'" "$f"
            [[ $PROXY_USER ]] && \
                in_chroot sed -i "'/^proxy/ a\proxy_username=$PROXY_USER'" "$f"
            [[ $PROXY_PASSWORD ]] && \
                in_chroot sed -i "'^/proxy_username/ a\proxy_password=$PROXY_PASSWORD'" "$f"
            : ;
        done
    )
}

setup_sledgehammer_chroot() {
    local repo rnum
    local packages=() pkg
    local files=() file
    local missing_pkgs=()
    local mirror="${EXTRA_REPOS[0]}"
    local -A base_pkgs
    # Build a hash of base packages. We will use this to track the packages we found in the mirror.
    for pkg in "${OS_BASIC_PACKAGES[@]}"; do
        base_pkgs["$pkg"]="needed"
    done
    # Fourth, get a list of packages in the mirror that we will use.
    match_re='^([A-Za-z0-9._+-]+)-([0-9]+:)?([0-9a-zA-Z._]+)-([^-]+)(\.el7.*)?\.(x86_64|noarch)\.rpm'
    while read file; do
        # Do we actaully care at all about this file?
        [[ $file =~ $match_re ]] || continue
        # Is this a file we need to download?
        [[ ${base_pkgs["${BASH_REMATCH[1]}"]} ]] || continue
        # It is. Mark it as found and put it in the list.
        base_pkgs["${BASH_REMATCH[1]}"]="found"
        files+=("-O" "${mirror}/Packages/$file")
    done < <(curl -sfL "${mirror}/Packages/" | \
        sed -rn 's/.*"([^"]+\.(x86_64|noarch).rpm)".*/\1/p')
    # Fifth, make sure we found all our packages.
    for pkg in "${!base_pkgs[@]}"; do
        [[ ${base_pkgs[$pkg]} = found ]] && continue
        missing_pkgs+=("$pkg")
    done
    if [[ $missing_pkgs ]]; then
        die "Not all files for CentOS chroot found." "${missing_pkgs[@]}"
    fi
    # Sixth, suck all of our files and install them in one go
    sudo mkdir -p "$CHROOT"
    (
        set -e
        set -o pipefail
        cd "$CHROOT"
        debug "Fetching files needed for chroot"
        curl -sfL "${files[@]}" || exit 1
        for file in filesystem*.rpm basesystem*.rpm *.rpm; do
            debug "Extracting $file"
            rpm2cpio "$file" | sudo cpio --extract --make-directories \
                --no-absolute-filenames --preserve-modification-time &>/dev/null
            if [[ $file =~ (centos|redhat)-release ]]; then
                sudo mkdir -p "$CHROOT/tmp"
                sudo cp "$file" "$CHROOT/tmp/${file##*/}"
                postcmds+=("/bin/rpm -ivh --force --nodeps /tmp/${file##*/}")
            fi
            rm "$file"
        done
        # Seventh, fix up the chroot so that it is fully functional.
        sudo cp /etc/resolv.conf "$CHROOT/etc/resolv.conf"
        for d in /proc /sys /dev /dev/pts /dev/shm; do
            nd=""
            [[ -L $d ]] && nd="$(readlink "$d")"
            if [ "$nd" != "" ] ; then
                d=$nd
            fi
            mkdir -p "${CHROOT}$d"
            sudo mount --bind "$d" "${CHROOT}$d"
        done
        # Eighth, run any post cmds we got earlier
        for cmd in "${postcmds[@]}"; do
            in_chroot $cmd
        done
    ) || die "Not all files needed for CentOS chroot downloaded."
    sudo rm --one-file-system -f "$CHROOT/etc/yum.repos.d/"*
    rnum=0
    for repo in "${EXTRA_REPOS[@]}"; do
        add_repos "bare r${rnum} 10 $repo"
        rnum=$((rnum + 1))
    done
    # Eleventh, bootstrap the rest of the chroot with yum.
    in_chroot yum -y install yum createrepo
    # fastestmirror support behind a proxy is not that good.
    [[ -f $CHROOT/etc/yum/pluginconf.d/fastestmirror.conf ]] && \
        sudo -H chroot "$CHROOT" /bin/sed -ie "/^enabled/ s/1/0/" \
        /etc/yum/pluginconf.d/fastestmirror.conf
    # Make sure yum does not throw away our caches for any reason.
    sudo -H chroot "$CHROOT" /bin/sed -i -e '/keepcache/ s/0/1/' /etc/yum.conf
    sudo -H chroot "$CHROOT" /bin/sh -c "echo 'exclude = *.i386' >>/etc/yum.conf"
    # fourth, have yum bootstrap everything else into usefulness
    chroot_install livecd-tools tar bsdtar bzip2 pciutils pciutils-devel zlib-devel gcc make
}

setup_sledgehammer_chroot
sudo cp "$REBAR_DIR/sledgehammer/"* "$CHROOT/mnt"
in_chroot mkdir -p /mnt/cache
sudo mount --bind "$SLEDGEHAMMER_LIVECD_CACHE" "$CHROOT/mnt/cache"
in_chroot touch /mnt/make_sledgehammer /mnt/stage1_init /mnt/udhcpc_config
in_chroot chmod 777 /mnt/make_sledgehammer /mnt/stage1_init /mnt/udhcpc_config
if [[ $USE_PROXY = "1" ]]; then
    printf "\nexport no_proxy=%q http_proxy=%q\n" \
        "$no_proxy" "$http_proxy" >> "$CHROOT/mnt/make_sledgehammer"
    printf "\nexport NO_PROXY=%q HTTP_PROXY=%q\n" \
        "$no_proxy" "$http_proxy" >> "$CHROOT/mnt/make_sledgehammer"
fi
sed "s/::SIGNATURE::/$signature/" >> "$CHROOT/mnt/stage1_init" <<"EOF"
#!/bin/ash

fail() {
    printf '%s\n' "$@"
    echo "Dropping into shell for debugging"
    echo "Contact RackN on Gitter in the digitalrebar/core channel for debugging"
    echo "Exiting the shell will reboot the system"
    /bin/ash -i
    reboot
}

export PATH=/bin
export SIGNATURE="::SIGNATURE::"
echo "Stage1 $SIGNATURE in Busybox"
echo "You can download a copy of the Busybox source for this version at:"
echo "http://opencrowbar.s3-website-us-east-1.amazonaws.com/busybox-source.tar.xz"
mount -t proc proc /proc
mount -t sysfs sysfs /sys
mount -t devtmpfs dev /dev
mount -t tmpfs root /newinitramfs
mkdir /dev/pts
mount -t devpts devpts /dev/pts
echo "Extracting drivers and other kernel modules"
xz -d -c -f lib.cpio.xz |cpio -id
test -d "lib/modules/$(uname -r)" || fail "Stage1 kernel modules do not match running kernel $(uname -r)" "We will fail to find network devices" "Make sure Sledgehammer $SIGNATURE was downloaded properly."
rm lib.cpio.xz
touch /dev/mdev.log /dev/mdev.seq
echo "Loading drivers"
echo '$MODALIAS=.* 0:0 660 @/bin/modprobe "$MODALIAS"' >/etc/mdev.conf
echo /bin/mdev >/proc/sys/kernel/hotplug
mdev -s
# Load devices not loaded by mdev -s
for i in /sys/class/net/*/uevent; do
    printf 'add' > "$i"; 
done 2>/dev/null; 
unset i
for i in /sys/bus/usb/devices/*; do
    case "${i##*/}" in
        [0-9]*-[0-9]*)
	    printf 'add' > "$i/uevent";;
    esac
done; unset i
# Load kernel modules, run twice.
find /sys -name 'modalias' -type f -exec cat '{}' + | sort -u | xargs -n 1 modprobe  2>/dev/null
find /sys -name 'modalias' -type f -exec cat '{}' + | sort -u | xargs -n 1 modprobe  2>/dev/null
# If we loaded mlx4_core, also load mlx4_en
if test -d /sys/module/mlx4_core; then
    modprobe mlx4_en
    mdev -s
fi
echo "Parsing kernel parameters required for booting"
bootif=$(grep -o 'BOOTIF=[^ ]*' /proc/cmdline)
nextone=$(grep -o 'provisioner.web=[^ ]*' /proc/cmdline)
test -z "$bootif" && fail "Missing required parameter BOOTIF"
test -z "$nextone" && fail "Missing required parameter provisioner.web"

bootif=${bootif#*01-}
bootif=${bootif//-/:}
nextone=${nextone#*=}

pxedev=""
for dev in /sys/class/net/*; do
    test -f "$dev/address" || continue
    if test "$(cat "$dev/address")" = "$bootif"; then
        pxedev=${dev##*/}
        break
    fi
done
test -z "$pxedev" && fail "Failed to find network device we booted from"
echo "Configuring boot interface"
ip link set "$pxedev" up
udhcpc -R -a -i "$pxedev"
echo "Fetching second-stage initramfs"
(cd /tmp; wget "$nextone/sledgehammer/$SIGNATURE/stage2.img") || \
    fail "Failed to download stage2.img for $SIGNATURE"
(cd /newinitramfs; cpio -id < /tmp/stage2.img)
test -f /newinitramfs/$SIGNATURE.squashfs || \
    fail "Stage2 image does not contain Stage2 squashfs"
rm /tmp/stage2.img
echo "Switching to second-stage initramfs"
modprobe loop
modprobe squashfs
mkdir /newinitramfs/.upper /newinitramfs/.work /newinitramfs/.lower
mount -o ro -t squashfs "/newinitramfs/$SIGNATURE.squashfs" /newinitramfs/.lower || \
    fail "Failed to mount $SIGNATURE.squashfs as Stage2 initramfs"
test -d "/newinitramfs/.lower/lib/modules/$(uname -r)" || \
    fail "Stage2 kernel modules do not match running kernel!"
# This is such a hack
insmod "/newinitramfs/.lower/lib/modules/$(uname -r)/kernel/fs/overlayfs/overlay.ko" || \
    fail "Failed to load overlayfs kernel module"
mount -t overlay -olowerdir=/newinitramfs/.lower,upperdir=/newinitramfs/.upper,workdir=/newinitramfs/.work newroot /newinitramfs || \
    fail "Failed to overlay a writeable filesystem over $SIGNATURE.squashfs"

for fs in /dev /dev/pts /proc /sys; do
    mkdir -p "/newinitramfs$fs"
    mount --bind "$fs" "/newinitramfs$fs"
done

echo /sbin/hotplug >/proc/sys/kernel/hotplug
pkill udhcpc
ip link set "$pxedev" down
exec switch_root /newinitramfs /sbin/init
fail "Failed to switch to stage2 initramfs" "This should never happen"
EOF

cat >> "$CHROOT/mnt/udhcpc_config" <<"EOF"
#!/bin/sh
# udhcpc script edited by Tim Riker <Tim@Rikers.org>

RESOLV_CONF="/etc/resolv.conf"

[ -n "$1" ] || { echo "Error: should be called from udhcpc"; exit 1; }

NETMASK=""
[ -n "$subnet" ] && NETMASK="netmask $subnet"
BROADCAST="broadcast +"
[ -n "$broadcast" ] && BROADCAST="broadcast $broadcast"

case "$1" in
    deconfig)
        echo "Setting IP address 0.0.0.0 on $interface"
        ifconfig $interface 0.0.0.0
        ;;

    renew|bound)
        echo "Setting IP address $ip on $interface"
        ifconfig $interface $ip $NETMASK $BROADCAST

        if [ -n "$router" ] ; then
            echo "Deleting routers"
            while route del default gw 0.0.0.0 dev $interface ; do
                :
            done

            metric=0
            for i in $router ; do
                echo "Adding router $i"
                route add default gw $i dev $interface metric $metric
                : $(( metric += 1 ))
            done
        fi

        echo "Recreating $RESOLV_CONF"
        # If the file is a symlink somewhere (like /etc/resolv.conf
        # pointing to /run/resolv.conf), make sure things work.
        realconf=$(readlink -f "$RESOLV_CONF" 2>/dev/null || echo "$RESOLV_CONF")
        tmpfile="$realconf-$$"
        > "$tmpfile"
        [ -n "$domain" ] && echo "search $domain" >> "$tmpfile"
        for i in $dns ; do
            echo " Adding DNS server $i"
            echo "nameserver $i" >> "$tmpfile"
        done
        mv "$tmpfile" "$realconf"
        ;;
esac

exit 0
EOF

sed "s/::SIGNATURE::/$signature/" >> "$CHROOT/mnt/make_sledgehammer" <<"EOF"
#!/bin/bash
set -e
set -x 
export PS4='${BASH_SOURCE}@${LINENO}(${FUNCNAME[0]}): '
export SIGNATURE="::SIGNATURE::"
(
    mkdir -p /mnt/flashrom
    cd /mnt/flashrom
    curl -fgL -O http://download.flashrom.org/releases/flashrom-0.9.9.tar.bz2
    tar xf flashrom-0.9.9.tar.bz2
    cd flashrom-0.9.9
    make CONFIG_ENABLE_LIBUSB0_PROGRAMMERS=no CONFIG_ENABLE_LIBUSB1_PROGRAMMERS=no
    make DESTDIR=/mnt/staged CONFIG_ENABLE_LIBUSB0_PROGRAMMERS=no CONFIG_ENABLE_LIBUSB1_PROGRAMMERS=no install
)
cd /mnt
livecd-creator --config=sledgehammer.ks --cache=./cache -f sledgehammer
rm -fr /mnt/tftpboot
bsdtar --strip-components=1 -xf sledgehammer.iso \
    isolinux/vmlinuz0 isolinux/initrd0.img LiveOS/squashfs.img
mkdir /mnt/tftpboot
mv vmlinuz0 "/mnt/tftpboot"
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
    local idx=${3:-0}
    decompress "$2">"$1/scratch.img" || {
        rm "$1/scratch.img"; return 1
    }
    local sep=$(grep -m 1 -P -o -a -b 'TRAILER!!!\x0*' "$1/scratch.img" |tr '\0' '-')
    local frag=${sep#*:}
    local startnext=$((${sep%%:*} + ${#frag}))
    local endfirst=$((${sep%%:*} + ${#frag} - 1))
    dd if="$1/scratch.img" count="$endfirst" iflag=count_bytes > "$1/ss$idx.img"
    idx=$((idx + 1))
    dd if="$1/scratch.img" skip="$startnext" iflag=skip_bytes > "$1/ss$idx.img"
    rm "$1/scratch.img"
}
cut_initramfs '.' initrd0.img

if [[ ! -f ss1.img ]]; then
   echo "Cutting initial initramfs apart failed"
   exit 1
fi
if ! grep -q 'early_cpio' ss0.img; then
    echo "First initramfs missing early_cpio"
    exit 1
fi
mkdir -p ss0/dev ss0/proc ss0/sys ss0/bin ss0/etc \
       ss0/usr/share/udhcpc ss0/tmp ss0/newinitramfs \
       ss1 stage1

echo "Making stage1.img"
(    cd ss0
     bsdtar -xf ../ss0.img || :
     bsdtar -xf ../ss1.img 'usr/lib/modules/*' 'usr/lib/firmware'
     mv usr/lib .
     (cd lib/modules/*/kernel/drivers; rm -rf ata md usb firewire scsi mmc cdrom gpu)
     (cd lib/firmware; rm -rf radeon)
     find lib |sort |cpio -o -R 0:0 --format=newc |xz -T0 -c >lib.cpio.xz
     rm -rf lib
     curl -fgL -o bin/busybox http://opencrowbar.s3-website-us-east-1.amazonaws.com/busybox
     (cd bin; chmod 755 busybox; ./busybox --install .)
     cp ../stage1_init init
     cp ../udhcpc_config usr/share/udhcpc/default.script
     find |sort |cpio -o -R 0:0 --format=newc |gzip -9 >/mnt/tftpboot/stage1.img
)
echo "Making stage2.img"
unsquashfs squashfs.img
mount -o loop,ro squashfs-root/LiveOS/ext3fs.img stage1
(cd stage1; tar cpf - .) | (cd ss1; tar xpf -)
umount stage1 && rmdir stage1
if [[ -d /mnt/staged ]]; then
    echo "Staging externally built stuff"
    (cd /mnt/staged; tar cfp - .) | (cd ss1; tar xpf -)
fi
mksquashfs ss1 $SIGNATURE.squashfs -comp xz -Xbcj x86
rm -rf ss1
echo $SIGNATURE.squashfs |cpio -o -R 0:0 --format=newc >/mnt/tftpboot/stage2.img

(cd /mnt/tftpboot; sha1sum vmlinuz0 *.img >sha1sums)
EOF
chmod 755 "$CHROOT/mnt/make_sledgehammer" "$CHROOT/mnt/stage1_init" "$CHROOT/mnt/udhcpc_config"
in_chroot ln -s /proc/self/mounts /etc/mtab
in_chroot /mnt/make_sledgehammer
cp -af "$CHROOT$SYSTEM_TFTPBOOT_DIR/"* "$SLEDGEHAMMER_IMAGE_DIR"
in_chroot rm -rf $SYSTEM_TFTPBOOT_DIR

if [[ -f $SLEDGEHAMMER_IMAGE_DIR/stage2.img ]]; then
    echo "New sledgehammer image in $SLEDGEHAMMER_IMAGE_DIR"
    echo "It has signature $signature"
    echo "To use it locally, modify PROV_SLEDGEHAMMER_SIG in deploy/compose/common.env to use it."
    exit 0
else
    exit 1
fi
