#!/bin/bash

supported_oses="$(rebar deployments get system attrib provisioner-supported-oses |jq -c '.value')"
declare -a available_oses
declare -A install_repos
iso_dir="${TFTPROOT}/isos"
rhelish_re='^(redhat|centos|fedora)'

# First, extract ISO files for operating systems we know about.
for key in $(jq -r 'keys[]' <<< "$supported_oses"); do
    os_specs="$(jq -r ".[\"$key\"]" <<< "$supported_oses")"
    iso="$(jq -r ".iso_file" <<< "$os_specs")"
    os_install_dir="${TFTPROOT}/$key/install"
    os_web_path="http://${EXTERNAL_IP%%/*}:${WEBPORT}/${key}/install"
    if [[ ! -f $os_install_dir/.${iso}.rebar_canary ]]; then
        if ! [[ $iso && -f $iso_dir/$iso ]]; then
            echo "$iso is not present, skipping"
            continue
        fi
        echo "Extracting $iso for $key"
        [[ -d "${os_install_dir}.extracting" ]] && rm -rf "${os_install_dir}.extracting"
        mkdir -p "${os_install_dir}.extracting"
        if [[ $key = esxi* ]]; then
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
        if [[ $key =~ $rhelish_re ]]; then
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
        echo "$iso already extracted for $key"
    fi
    available_oses+=("$key")
    case $key in
        ubuntu*)
            if [[ -d $os_install_dir/dists/stable ]]; then
                install_repos["$key"]="[\"deb $os_web_path stable main restricted\"]"
            fi;;
        suse*)
            install_repos["$key"]="[\"bare $os_web_path\"]";;
        redhat*|centos*|fedora*)
            if [[ -d $os_install_dir/repodata ]]; then
               install_repos["$key"]="[\"bare $os_web_path\"]"
            elif [[ -d $os_install_dir/Server/repodata ]]; then
                install_repos["$key"]="[\"bare $os_web_path/Server\"]"
            fi;;
    esac
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

rebar deployments set system attrib provisioner-available-oses to "$avail_os_line"

