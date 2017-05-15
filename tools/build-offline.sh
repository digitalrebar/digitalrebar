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
    echo "Archiving to digitalrebar-offline-bits.tar"
    cd "$tmpdir";
    tar cf digitalrebar-offline-bits.tar sledgehammer digitalrebar.git rebar-containers.tar.xz
)
mv "$tmpdir/digitalrebar-offline-bits.tar" .
echo "Cleaning up"
rm -rf "$tmpdir"
