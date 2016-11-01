#!/bin/bash

echo "Explode iso $1 $2 $3"

rhelish_re='^(redhat|centos|fedora)'

os_name=$1
iso=$2
os_install_dir=$3

echo "Extracting $iso for $os_name"
[[ -d "${os_install_dir}.extracting" ]] && rm -rf "${os_install_dir}.extracting"
mkdir -p "${os_install_dir}.extracting"
case $os_name in
    esxi*)
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
            # ESX needs an exact version of pxelinux, so add it.
            tar xf /tmp/syslinux-3.86.tar.xz syslinux-3.86/core/pxelinux.0 -O > pxelinux.0
        );;
    windows*)
        # bsdtar does not extract the UDF part of the ISO image, so
        # we will use 7zip to do it.
        (
            cd "${os_install_dir}.extracting"
            7z x "${iso}"
            # Windows needs wimboot, so extract it.  This must be kept in sync with
            # the version of wimboot we have made available in the Dockerfile.
            tar xf /tmp/wimboot-2.5.2.tar.bz2 wimboot-2.5.2/wimboot -O > wimboot
            # Fix up permissions so things can execute
            chmod -R 555 .
        )
        ;;
        
    *)
        # Everything else just needs bsdtar
        (cd "${os_install_dir}.extracting"; bsdtar -x -f "${iso}");;
esac
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

if which selinuxenabled && selinuxenabled; then
    restorecon -R -F /tftpboot
fi

