#!/bin/bash

set -e

if ! [[ $PWD/tools/build-offline.sh ]]; then
    echo "build-offline.sh must be run from the root of the Digital Rebar checkout"
    exit 1
fi

tmpdir="$(mktemp -d /tmp/rebar-offline-XXXXXX)" || exit 1
# Export our containers
echo "Exporting Digital Rebar containers"
"./containers/export-containers" "$tmpdir/rebar-containers.tar.xz"
echo "Copying the Digital Rebar source code"
git clone --bare --no-hardlinks . "$tmpdir/digitalrebar.git"
(cd "$tmpdir/digitalrebar.git"; git repack -a -d && git prune)
echo "Fetching the current version of Sledgehammer"
(
    . deploy/compose/common.env
    cacheloc="$HOME/.cache/digitalrebar/tftpboot/sledgehammer/$PROV_SLEDGEHAMMER_SIG"
    dest="sledgehammer/$PROV_SLEDGEHAMMER_SIG"
    mkdir -p "$tmpdir/$dest" "$cacheloc"
    for f in sha1sums vmlinuz0 stage1.img stage2.img; do
        if ! [[ -d $cacheloc && -f $cacheloc/$f ]]; then
            (cd "$cacheloc" && curl -fgLO "$PROV_SLEDGEHAMMER_URL/$PROV_SLEDGEHAMMER_SIG/$f")
        fi
        cp "$cacheloc/$f" "$tmpdir/$dest/$f"
    done
)
(
    cd "$tmpdir"
    echo "Creating initial install script"
    cat >initial-install.sh <<"EOF"
#!/usr/bin/env bash
set -e
set -o pipefail

srcdir="$PWD"

if ! [[ -f rebar-containers.tar.xz && -d sledgehammer && -d digitalrebar.git ]]; then
    echo "This script must be run where digitalrebar-offline-bits.tar was extracted"
    exit 1
fi

srcdest="/opt/digitalrebar"
tftpdest="/var/lib/tftpboot"

err=0

if ! which docker &>/dev/null; then
    echo "Docker is not installed."
    echo "A current docker version is required to run Digital Rebar."
    echo "You can install Docker from:"
    echo "https://store.docker.com/search?type=edition&offering=community"
    echo
    err=1
fi

if ! which docker-compose &>/dev/null; then
    echo "docker-compose is not installed."
    echo "A current version of docker-compose is required to run Digital Rebar"
    echo "You can download the docker-compose binary for your OS from"
    echo "https://github.com/docker/compose/releases/download/1.13.0/docker-compose-$(uname -s)-$(uname -m)"
    echo
    err=1
fi

if ! which git &>/dev/null; then
    echo "git is not installed."
    echo "A current version of git is required to install Digital Rebar"
    echo "Use the package manager to install git."
    echo
    err=1
fi

if [[ $err == 1 ]] ; then
    exit 1
fi

if ! [[ -d $srcdest ]]; then
    echo "Performing initial install of Digital Rebar source into $srcdest"
    mkdir -p "$srcdest"
    (cd "$srcdest"; git clone --no-hardlinks "$srcdir/digitalrebar.git" .)
    echo
else
    echo "Digital Rebar source already installed.  Nothing else to do."
    exit 0
fi

echo "Installing Sledgehammer discovery and configuration image"
mkdir -p "$tftpdest"
cp -f -a -t "$tftpdest" sledgehammer

echo
echo "Loading initial containers"
/opt/digitalrebar/containers/load-containers "$srcdir/rebar-containers.tar.xz"
EOF
    chmod 755 initial-install.sh
    echo "Archiving to digitalrebar-offline-bits.tar"
    tar cf digitalrebar-offline-bits.tar initial-install.sh sledgehammer digitalrebar.git rebar-containers.tar.xz
)
mv "$tmpdir/digitalrebar-offline-bits.tar" .
echo "Cleaning up"
rm -rf "$tmpdir"
