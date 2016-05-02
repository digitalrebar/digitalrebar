#!/bin/bash

rhelish_re='^(redhat|centos|fedora)'

os_name=$1
iso=$2
os_install_dir=$3

echo "Extracting $iso for $os_name"
[[ -d "${os_install_dir}.extracting" ]] && rm -rf "${os_install_dir}.extracting"
mkdir -p "${os_install_dir}.extracting"
if [[ $os_name = esxi* ]]; then
    # ESXi needs some special love extracting the files from the image.
    # Note that this is likely to fail inside the container.
    tmpdir="$(mktemp -d "/tmp/esx-XXXXXXXX")"
    mount -o loop "$iso" "$tmpdir"
    rsync -av "$tmpdir" "${os_install_dir}.extracting"
    sync
    umount "$tmpdir"
    rm -rf "$tmpdir"
    chmod +w "${os_install_dir}.extracting"/*
    sed -e "s:/::g" -e "3s:^:prefix=/../${key}/install/\\n:" -i.bak "${os_install_dir}.extracting"/boot.cfg
else
    # Everything else just needs bsdtar
    (cd "${os_install_dir}.extracting"; bsdtar -x -f "${iso}")
fi
if [[ $os_name =~ $rhelish_re ]]; then
    # Rewrite local package metadata
    (
        cd "${os_install_dir}.extracting"
        groups=($(echo repodata/*comps*.xml))
        createrepo -g "${groups[-1]}" .
    )
fi
touch "${os_install_dir}.extracting/.${iso}.rebar_canary"
[[ -d "${os_install_dir}" ]] && rm -rf "${os_install_dir}"
mv "${os_install_dir}.extracting" "${os_install_dir}"

if which selinuxenabled && selinuxenabled; then
    restorecon -R -F /tftpboot
fi

