#!/bin/bash

set -x
[[ -f /tmp/.repo_update ]] && rm /tmp/.repo_update

case $OS_TYPE in
    rhel|redhat|fedora|centos|scientificlinux)
        mkdir -p /etc/yum.repos.d
        [[ -f /etc/yum/pluginconf.d/fastestmirror.conf ]] && \
            sed -i '/^enabled/ s/1/0/' /etc/yum/pluginconf.d/fastestmirror.conf;;
    debian|ubuntu)
        mkdir -p /etc/apt/sources.list.d /etc/apt/apt.conf.d
        cat >/etc/apt/apt.conf.d/99-rebar-allow-unauthenticated <<EOF
APT::Get::AllowUnauthenticated 1 ;
Dpkg::Options {
   "--force-confdef";
   "--force-confnew";
}
EOF
        ;;
    alpine);;
    *)
        echo "No idea how to handle $OS_TYPE"
        exit 1;;
esac

declare -A pkg_sources

if [[ $(read_attribute 'rebar/providers/use_proxy') = true ]]; then
    case $OS_TYPE in
        rhel|redhat|fedora|centos|scientificlinux)
            if ! grep -F -q "proxy=http" /etc/yum.conf; then
                echo "proxy=${http_proxy}" >> /etc/yum.conf
            else
                sed -i "/^proxy/ s@http://.*@${http_proxy}@" /etc/yum.conf
            fi;;
        debian|ubuntu)
            printf 'Acquire::http::Proxy "%s";\n' "$http_proxy" > /etc/apt/apt.conf.d/00-proxy;;

        alpine);;
    esac
fi

shait() {
    local l rest
    read -r l rest < <(sha256sum -b "$1")
    printf '%s' "$l"
}

repositories="$(get_attr 'package-repositories')"

for repo_name in $(jq -r -c 'keys | .[]' <<< "$repositories"); do
    for target_os in "${__os_alts[@]}"; do
        printf -v contents '%s' "$(jq -r ".[\"$repo_name\"] | .oses | .[\"${target_os}\"] | .contents" <<< "$repositories")"
        [[ $contents = null || $contents = '' ]] && continue
        priority="$(jq -r -c ".[\"$repo_name\"] | .priority" <<< "$repositories")"
        disabled="$(jq -r -c ".[\"$repo_name\"] | .disabled" <<< "$repositories")"
        repo_file="${priority}-rebar-${repo_name}"
        if [[ $disabled = true ]]; then
            case $OS_FAMILY in
                rhel) rm "/etc/yum.repos.d/$repo_file.repo" || : ;;
                debian) rm "/etc/apt/sources.list.d/$repo_file.list" || : ;;
                *)
                    echo "No idea how to clean up repo definitions for $OS_TYPE"
                    exit 1;;
            esac
            touch /tmp/.repo_update
        else

            case $OS_FAMILY in
                rhel) repo_path="/etc/yum.repos.d/$repo_file.repo";;
                debian) repo_path="/etc/apt/sources.list.d/$repo_file.list";;
                *)
                    echo "No idea how to create repo definitions for $OS_TYPE"
                    exit 1;;
            esac
            mkdir -p "${repo_path%/*}"
            tmp_repo=$(mktemp --tmpdir repo_XXXXXX)
            printf >"$tmp_repo" '%b' "$contents"
            if [[ ! -f $repo_path || $(shait "$tmp_repo") != $(shait "${repo_path}") ]]; then
                touch /tmp/.repo_update
                mv "$tmp_repo" "$repo_path"
            fi
        fi
        break
    done
done

if [[ -f /tmp/.repo_update ]]; then
    case $OS_TYPE in
        rhel|redhat|fedora|centos|scientificlinux) yum clean expire-cache;;
        debian|ubuntu) apt-get -y update;;

    esac
    rm /tmp/.repo_update
fi
