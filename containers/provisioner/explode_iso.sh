#!/bin/bash

echo "Explode iso $1 $2 $3"

rhelish_re='^(redhat|centos|fedora)'

os_name=$1
iso=$2
os_install_dir=$3

echo "Extracting $iso for $os_name"
[[ -d "${os_install_dir}.extracting" ]] && rm -rf "${os_install_dir}.extracting"
mkdir -p "${os_install_dir}.extracting"
if [[ $os_name = esxi* ]]; then
    # ESXi needs some special love extracting the files from the image.
    # Specifically, bsdtar extracts everything in UPPERCASE,
    # where everything else expects lowercase
    (
        cd "${os_install_dir}.extracting"
        bsdtar -x -f "${iso}"
        changed=true
        while [[ $changed = true ]]; do
            changed=false
            while read d; do
                [[ $d = . || $d = ${d,,} ]] && continue
                mv "$d" "${d,,}"
                changed=true
            done < <(find . -type d |sort)
        done
        while read d; do
            [[ $d = ${d,,} ]] && continue
            mv "${d}" "${d,,}"
        done < <(find . -type f |sort)
    )
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
touch "${os_install_dir}.extracting/.${os_name}.rebar_canary"
[[ -d "${os_install_dir}" ]] && rm -rf "${os_install_dir}"
mv "${os_install_dir}.extracting" "${os_install_dir}"

# ESX needs an exact version of pxelinux, so add it.
if [[ ! -f ${os_install_dir}/pxelinux.0 ]]; then
    tar xJf /tmp/syslinux-3.86.tar.xz syslinux-3.86/core/pxelinux.0 -O >\
        "${os_install_dir}/pxelinux.0"
fi

if which selinuxenabled && selinuxenabled; then
    restorecon -R -F /tftpboot
fi

