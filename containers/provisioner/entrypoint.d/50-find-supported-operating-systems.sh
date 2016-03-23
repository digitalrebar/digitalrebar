#!/bin/bash

declare -a available_oses
declare -A install_repos
iso_dir="${TFTPROOT}/isos"
rhelish_re='^(redhat|centos|fedora)'
declare -A preferred_oses
preferred_oses['centos-7.2.1511']=0
preferred_oses['centos-7.1.1503']=1
preferred_oses['ubuntu-14.04']=2
preferred_oses['ubuntu-15.04']=3
preferred_oses['debian-8']=4
preferred_oses['centos-6.6']=5
preferred_oses['debian-7']=6
preferred_oses['redhat-6.5']=7
preferred_oses['ubuntu-12.04']=8

available_oses=()
default_os='fakey-mcfakerson'
os_preference=999
# First, extract ISO files for operating systems we know about.
for f in "$GOPATH/src/github.com/rackn/provisioner-mgmt/bootenvs"/*.json; do
    bootenv_name=$(jq -r '.Name' < "$f")
    [[ $bootenv_name = *-install ]] || continue
    os_name=$(jq -r '.OS.Name' < "$f")
    [[ $os_name && $os_name != null ]] || continue
    os_install_dir="${TFTPROOT}/$os_name/install"
    iso="$(jq -r ".OS.IsoFile" < "$f")"
    os_web_path="http://${EXTERNAL_IP%%/*}:${WEBPORT}/${os_name}/install"
    if [[ ! -f $os_install_dir/.${iso}.rebar_canary ]]; then
        if ! [[ $iso && -f $iso_dir/$iso ]]; then
            echo "$iso is not present, skipping"
            continue
        fi
        iso_shasum=$(jq -r '.OS.IsoSha256' < "$f")
        if [[ $iso_shasum && $iso_shasum != null ]]; then
            echo "Validating checksum for $ISO"
            if ! (cd "$iso_dir"; sha256sum -c <(printf '%s  %s\n' "$iso_shasum" "$iso")); then
                echo "ISO $iso download interrupted or corrupt -- checksums do not match"
                continue
            fi
        fi
        echo "Extracting $iso for $os_name"
        [[ -d "${os_install_dir}.extracting" ]] && rm -rf "${os_install_dir}.extracting"
        mkdir -p "${os_install_dir}.extracting"
        if [[ $os_name = esxi* ]]; then
            # ESXi needs some special love extracting the files from the image.
            # Note that this is likely to fail inside the container.
            tmpdir="$(mktemp -d "/tmp/esx-XXXXXXXX")"
            mount -o loop "$iso_dir/$iso" "$tmpdir"
            rsync -av "$tmpdir" "${os_install_dir}.extracting"
            sync
            umount "$tmpdir"
            rm -rf "$tmpdir"
            chmod +w "${os_install_dir}.extracting"/*
            sed -e "s:/::g" -e "3s:^:prefix=/../${key}/install/\\n:" -i.bak "${os_install_dir}.extracting"/boot.cfg
        else
            # Everything else just needs bsdtar
            (cd "${os_install_dir}.extracting"; bsdtar -x -f "${iso_dir}/${iso}")
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
    else
        echo "$iso already extracted for $os_name"
    fi
    available_oses+=("$os_name")
    case $os_name in
        ubuntu*)
            if [[ -d $os_install_dir/dists/stable ]]; then
                install_repos["$os_name"]="[\"deb $os_web_path stable main restricted\"]"
            fi;;
        suse*)
            install_repos["$os_name"]="[\"bare $os_web_path\"]";;
        redhat*|centos*|fedora*)
            if [[ -d $os_install_dir/repodata ]]; then
               install_repos["$os_name"]="[\"bare $os_web_path\"]"
            elif [[ -d $os_install_dir/Server/repodata ]]; then
                install_repos["$os_name"]="[\"bare $os_web_path/Server\"]"
            fi;;
    esac
    if ((os_preference > ${preferred_oses["$os_name"]})); then
        os_preference=${preferred_oses["$os_name"]}
        default_os="$os_name"
    fi
done

if [[ $available_oses ]]; then
    avail_os_line="$(printf '"%s":true,' "${available_oses[@]}")"
    avail_os_line="{\"value\": { ${avail_os_line%,} }}"
else
    avail_os_line='{"value": {} }'
fi

if which selinuxenabled && selinuxenabled; then
    restorecon -R -F ${TFTPROOT}
fi

set_service_attrib provisioner-service provisioner-available-oses "$avail_os_line"
set_service_attrib provisioner-service provisioner-default-os "{\"value\": \"$default_os\"}"
