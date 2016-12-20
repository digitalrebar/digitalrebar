#!/bin/bash

[[ -f /tmp/.repo_update ]] && rm /tmp/.repo_update

case $OS_TYPE in
    rhel|redhat|fedora|centos|scientificlinux)
        mkdir -p /etc/yum.repos.d
        [[ -f /etc/yum/pluginconf.d/fastestmirror.conf ]] && \
            sed -i '/^enabled/ s/1/0/' /etc/yum/pluginconf.d/fastestmirror.conf
        if [[ $OS_TYPE = centos && ! -f /etc/yum.repos.d/CentOS-Base.repo ]]; then
            yum -y reinstall centos-release
            touch /tmp/.repo_update
        fi;;
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

while read line; do
    repo_name="$(jq -r -c '.name' <<< "$line")"
    repo_urls=()
    while read os_line; do
        this_os="$(jq -r -c '.os' <<< "$os_line")"
        [[ $OS_NAME = ${this_os}* ]] || continue
        while read url; do
            repo_urls+=("$url")
        done < <(jq -r '.repos|.[]' <<< "$os_line")
    done < <(jq -r -c '.oses|.[]' <<< "$line")
    [[ $repo_urls ]] || continue
    pkg_sources[$repo_name]="$(printf '%s\n' "${repo_urls[@]}")"
done < <(read_attribute 'rebar/package_repos' |jq -r -c '.[]')

for repo_name in "${!pkg_sources[@]}"; do
    while read url; do
        read head rest <<< "$url"
        case $head in
            deb*)
                if ! grep -q -F "$url" "/etc/apt/sources.list.d/10-rebar-${repo_name}.list"; then
                    printf '%s\n' "$url" >> "/etc/apt/sources.list.d/10-rebar-${repo_name}.list"
                    touch /tmp/.repo_update
                fi;;
            rpm)
                if [[ ! -f /var/cache/${rest##*/} ]]; then
                    (cd /var/cache && curl -fgl -O "$rest")
                    if ! (cd /var/cache && rpm -qa | fgrep -q "$(rpm -qp ${rest##*/})") ; then
                        (cd /var/cache && rpm -Uvh "${rest##*/}")
                    fi
                    touch /tmp/.repo_update
                fi;;
            bare)
                if ! grep -q -F "$rest" "/etc/yum.repos.d/rebar-${repo_name}.repo"; then
                    cat > "/etc/yum.repos.d/rebar-${repo_name}.repo" <<EOF
[rebar-${repo_name}]
name=Rebar ${repo_name} Repo
baseurl=${rest}
gpgcheck=0
EOF
                    touch /tmp/.repo_update
                fi;;
            *)
                echo "No idea how to handle $head"
                exit 1;;
        esac
    done <<< "${pkg_sources[$repo_name]}"
done

if [[ -f /tmp/.repo_update ]]; then
    case $OS_TYPE in
        rhel|redhat|fedora|centos|scientificlinux) yum clean expire-cache;;
        debian|ubuntu) apt-get -y update;;

    esac
    rm /tmp/.repo_update
fi
