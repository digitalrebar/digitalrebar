#!/bin/bash
#
# Build a sledgehammer image for Crowbar and put it in the build cache.

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
[[ $CACHE_DIR ]] || CACHE_DIR="$HOME/.cache/opencrowbar/sledgehammer"
[[ $SLEDGEHAMMER_PXE_DIR ]] || SLEDGEHAMMER_PXE_DIR="$HOME/.cache/opencrowbar/tftpboot/discovery"
[[ $SLEDGEHAMMER_ARCHIVE ]] || SLEDGEHAMMER_ARCHIVE="$HOME/.cache/opencrowbar/tftpboot/sledgehammer"
[[ $CHROOT ]] || CHROOT="$CACHE_DIR/chroot"
[[ $SLEDGEHAMMER_LIVECD_CACHE ]] || SLEDGEHAMMER_LIVECD_CACHE="$CACHE_DIR/livecd_cache"
[[ $SYSTEM_TFTPBOOT_DIR ]] || SYSTEM_TFTPBOOT_DIR="/mnt/tftpboot"

CROWBAR_DIR="${0%/*}/.."

signature=$(sha1sum < <(cat "$0" "$CROWBAR_DIR/sledgehammer/"*) |awk '{print $1}')
SLEDGEHAMMER_IMAGE_DIR="$SLEDGEHAMMER_ARCHIVE/$signature"

sudo rm -rf "$CHROOT"

mkdir -p "$CACHE_DIR" "$CHROOT" "$SLEDGEHAMMER_PXE_DIR" \
    "$SLEDGEHAMMER_IMAGE_DIR" "$SLEDGEHAMMER_LIVECD_CACHE"

if ! which cpio &>/dev/null; then
    die "Cannot find cpio, we cannot proceed."
fi

if ! which rpm rpm2cpio &>/dev/null; then
    die "Cannot find rpm and rpm2cpio, we cannot proceed."
fi

OS_BASIC_PACKAGES=(MAKEDEV upstart audit-libs basesystem bash binutils \
    bzip2-libs chkconfig cracklib cracklib-dicts crontabs coreutils db4 \
    device-mapper e2fsprogs e2fsprogs-libs elfutils-libelf ethtool expat \
    file-libs filesystem findutils gawk gdbm glib2 glibc glibc-common grep \
    info initscripts iputils keyutils-libs krb5-libs libacl libattr libcap \
    libcom_err libgcc libidn libselinux libsepol libstdc++ libsysfs libgcrypt \
    libnih dbus-libs libcurl curl lua compat-libtermcap libutempter libxml2 \
    libxml2-python logrotate m2crypto mcstrans mingetty mlocate \
    module-init-tools ncurses ncurses-libs neon net-tools nss nss-sysinit \
    nss-softokn nss-softokn-freebl openldap libssh2 cyrus-sasl-lib nss-util \
    nspr openssl pam passwd libuser pcre popt procps psmisc python \
    python-libs python-pycurl python-iniparse python-urlgrabber readline rpm \
    rpm-libs rpm-python sed setup shadow-utils centos-release \
    sqlite rsyslog tzdata udev util-linux-ng xz xz-libs yum \
    yum-plugin-downloadonly yum-metadata-parser yum-utils zlib)

EXTRA_REPOS=('http://mirror.centos.org/centos/6/os/x86_64' \
    'http://mirror.centos.org/centos/6/updates/x86_64' \
    'http://mirror.centos.org/centos/6/extras/x86_64' \
    'http://mirror.us.leaseweb.net/epel/6/x86_64' \
    'http://www.nanotechnologies.qc.ca/propos/linux/centos-live/x86_64/live' \
    'http://rbel.frameos.org/stable/el6/x86_64' \
    'http://download.opensuse.org/repositories/Openwsman/CentOS_CentOS-6')

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
    printf "%s\n" "$*" >&2
    exit 1
}

debug() {
    printf "%s\n" "$*" >&2
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

cleanup() {
    set +e
    # Make sure that the loopback kernel module is loaded.
    [[ -e /dev/loop0 ]] || sudo modprobe loop

    while read line; do
        sudo losetup -d "${line%%:*}"
    done < <(sudo losetup -a |grep sledgehammer.iso)

    while read dev fs type opts rest; do
        sudo umount -d -l "$fs"
    done < <(tac /proc/self/mounts |grep -e "$CHROOT")
}

trap cleanup EXIT INT QUIT TERM

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
    local mirror="${EXTRA_REPOS[0]}"
    local -A base_pkgs
    # Build a hash of base packages. We will use this to track the packages we found in the mirror.
    for pkg in "${OS_BASIC_PACKAGES[@]}"; do
        base_pkgs["$pkg"]="needed"
    done
    # Fourth, get a list of packages in the mirror that we will use.
    match_re='^([A-Za-z0-9._+-]+)-([0-9]+:)?([0-9a-zA-Z._]+)-([^-]+)(\.el6.*)?\.(x86_64|noarch)\.rpm'
    while read file; do
        # Do we actaully care at all about this file?
        [[ $file =~ $match_re ]] || continue
        # Is this a file we need to download?
        [[ ${base_pkgs["${BASH_REMATCH[1]}"]} ]] || continue
        # It is. Mark it as found and put it in the list.
        base_pkgs["${BASH_REMATCH[1]}"]="found"
        files+=("-O" "${mirror}/Packages/$file")
    done < <(curl -sfL "{$mirror}/Packages/" | \
        sed -rn 's/.*"([^"]+\.(x86_64|noarch).rpm)".*/\1/p')
    # Fifth, make sure we found all our packages.
    for pkg in "${base_pkgs[@]}"; do
        [[ $pkg = found ]] && continue
        die "Not all files for CentOS chroot found."
    done
    # Sixth, suck all of our files and install them in one go
    sudo mkdir -p "$CHROOT"
    (
        set -e
        set -o pipefail
        cd "$CHROOT"
        debug "Fetching files needed for chroot"
        curl -sfL "${files[@]}" || exit 1
        for file in *.rpm; do
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
            [[ -L $d ]] && d="$(readlink -f "$d")"
            mkdir -p "${CHROOT}$d"
            sudo mount --bind "$d" "${CHROOT}$d"
        done
        # Eighth, run any post cmds we got earlier
        for cmd in "${postcmds[@]}"; do
            in_chroot $cmd
        done
    ) || die "Not all files needed for CentOS chroot downloaded."
    sudo rm -f "$CHROOT/etc/yum.repos.d/"*
    rnum=0
    for repo in "${EXTRA_REPOS[@]}"; do
        add_repos "bare r${rnum} 10 $repo"
        rnum=$((rnum + 1))
    done
    # Eleventh, bootstrap the rest of the chroot with yum.
    in_chroot yum -y install yum yum-downloadonly createrepo
    # fastestmirror support behind a proxy is not that good.
    [[ -f $CHROOT/etc/yum/pluginconf.d/fastestmirror.conf ]] && \
        sudo -H chroot "$CHROOT" /bin/sed -ie "/^enabled/ s/1/0/" \
        /etc/yum/pluginconf.d/fastestmirror.conf
    # Make sure yum does not throw away our caches for any reason.
    sudo -H chroot "$CHROOT" /bin/sed -i -e '/keepcache/ s/0/1/' /etc/yum.conf
    sudo -H chroot "$CHROOT" /bin/sh -c "echo 'exclude = *.i386' >>/etc/yum.conf"
    # fourth, have yum bootstrap everything else into usefulness
    chroot_install livecd-tools tar
}

setup_sledgehammer_chroot
sudo cp "$CROWBAR_DIR/sledgehammer/"* "$CHROOT/mnt"
in_chroot mkdir -p /mnt/cache
sudo mount --bind "$SLEDGEHAMMER_LIVECD_CACHE" "$CHROOT/mnt/cache"
in_chroot touch /mnt/make_sledgehammer
in_chroot chmod 777 /mnt/make_sledgehammer
echo '#!/bin/bash' >>"$CHROOT/mnt/make_sledgehammer"
if [[ $USE_PROXY = "1" ]]; then
    printf "\nexport no_proxy=%q http_proxy=%q\n" \
        "$no_proxy" "$http_proxy" >> "$CHROOT/mnt/make_sledgehammer"
    printf "\nexport NO_PROXY=%q HTTP_PROXY=%q\n" \
        "$no_proxy" "$http_proxy" >> "$CHROOT/mnt/make_sledgehammer"
fi
cat >> "$CHROOT/mnt/make_sledgehammer" <<EOF
set -e
cd /mnt
livecd-creator --config=sledgehammer.ks --cache=./cache -f sledgehammer
rm -fr $SYSTEM_TFTPBOOT_DIR
livecd-iso-to-pxeboot sledgehammer.iso
(cd "$SYSTEM_TFTPBOOT_DIR"; sha1sum vmlinuz0 initrd0.img >sha1sums)
rm /mnt/sledgehammer.iso
EOF
in_chroot ln -s /proc/self/mounts /etc/mtab
in_chroot /mnt/make_sledgehammer
cp -af "$CHROOT$SYSTEM_TFTPBOOT_DIR/"* "$SLEDGEHAMMER_IMAGE_DIR"
in_chroot rm -rf $SYSTEM_TFTPBOOT_DIR

if [[ -f $SLEDGEHAMMER_IMAGE_DIR/initrd0.img ]]; then
    echo "New sledgehammer image in $SLEDGEHAMMER_IMAGE_DIR"
    echo "It has signature $signature"
    echo "To use it locally, modify the bootstrap recipe to refer to the new signature."
    exit 0
else
    exit 1
fi
