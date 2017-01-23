#!/usr/bin/env bash

hex_re='^[0-9a-fA-F]+$'

# return 255 if $1 < $2, 0 if $1 = $2, 1 if $1 > $2 
rev_eq() {
    local val_a=() val_b=()
    local i
    IFS='.-_' read -a val_a <<< "$1"
    IFS='.-_' read -a val_b <<< "$2"
    for ((i=0; i<${#val_a[@]}; i++)); do
        if [[ ${val_a[$i]} =~ $hex_re  && ${val_b[$i]} =~ $hex_re ]]; then
            (( 0x${val_a[$i]} == 0x${val_b[$i]} )) && continue
            (( 0x${val_a[$i]} < 0x${val_b[$i]} )) && return 255 || return 1
        else
            [[ ${val_a[$i]} = ${val_b[$i]} ]] && continue
            [[ ${val_a[$i]} < ${val_b[$i]} ]] && return 255 || return 1
        fi
    done
    # One day handle dissimilar array lengths
    return 0
}

rev_ne() {
    ! rev_ne "$1" "$2"
}

rev_lt() {
    rev_eq "$1" "$2"
    (($? == 255))
}

rev_lte() {
    rev_eq "$1" "$2" || (($? == 255))
}

rev_gt() {
    rev_eq "$1" "$2"
    (($? == 1))
}

rev_gte() {
    rev_eq "$1" "$2" || (($? == 1))
}

if [[ $(get_attr 'skip-flash') == true ]]; then
    echo "Skipping all flash operations because skip-flash is true"
    exit 0
fi

target_versions="$(get_attr firmware-target-versions)"
packages_to_run=()
firmware_tests="$(get_attr firmware-selection-tests)"
# Iterate over all the tests in order
while read -r fw_test; do
    name="$(jq -r -c '.test' <<< "$fw_test")"
    match_json="$(jq -r -c '.match' <<< "$fw_test")"
    matched=true
    # The current test should only pass if all the match attribs are
    # a perfect match.
    while read -r attrib; do
        test_val="$(jq -r -c ".[\"${attrib}\"]" <<< "$match_json")"
        [[ $test_val = $(get_attr "$attrib") ]] || matched=false
    done < <(jq -r -c 'keys' <<<"$match_json" |jq -r -c '.[]')
    if [[ $matched = false ]]; then
       echo "$name not applicable, skipping"
       continue
    fi
    # Get the current and target versions for this test
    printf '%b' "$(jq -r '.["current-version-script"]' <<<"$fw_test")" > vchk.sh
    read -r current_version <<< "$(. ./vchk.sh)"
    target_version="$(jq -r -c ".[\"$name\"]" <<< "$target_versions")"
    if [[ $target_version = ignore ]]; then
        echo "$name is being ignored by user request"
        continue
    fi
    [[ $target_version = latest || $target_version = null ]] && target_version=''
    pkgs="$(jq -r -c '.packages' <<< "$fw_test")"

    if [[ ! $target_version ]]; then
        # Walk up each of the packages.  After we hit ones that are
        # greater than the current version, flash each one that has
        # upgrade_fence set and the last one we find.
        while read -r pkg; do
            pkg_ver="$(jq -r -c '.version' <<< "$pkg")"
            rev_lte "$pkg_ver" "$current_version" && continue
            pkg_tgt="$(jq -r -c '.package' <<< "$pkg")"
            [[ $(jq -r -c '.["upgrade-fence"]' <<< "$pkg") = true ]] && break
        done < <(jq -r -c '.[]' <<< "$pkgs")
    elif rev_le "$target_version" "$current_version"; then
        # Walk down the list of packages.  If we find the target version
        # before we hit a version with downgrade-fence or we run out of
        # packages, flash the found package.
        while read -r pkg; do
            pkg="${pkgs[$i]}"
            pkg_ver="$(jq -r -c '.version' <<< "$pkg")"
            rev_gte "$pkg_ver" "$current_version" && continue
            if rev_lt "$pkg_ver" "$target_version"; then
                echo "Cannot downgrade $name to $target_version"
                echo "It is not in the packages for $nane in firmware-selection-tests"
                exit 1
            fi
            if rev_eq "$pkg_ver" "$target_version"; then
                export downgrade=true
                pkg_tgt="$(jq -r -c '.package' <<< "$pkg")"
                break
            fi
            if [[ $(jq -r -c '.["downgrade-fence"]' <<< "$pkg") = true ]]; then
                echo "Cannot downgrade $name to $target_version"
                echo "$name $pkg_ver introduced non-reversible changes"
                exit
            fi
        done < <(jq 'reverse' <<< "$pkgs" | jq -r -c '.[]')
    elif rev_gt "$target_version" "$current_version"; then
        # Walk up the list of package as if target_version is not set, but stop
        # if we hit something greater than the target version.
        while read -r pkg; do
            pkg_ver="$(jq -r -c '.version' <<< "$pkg")"
            rev_lt "$pkg_ver" "$current_version" && continue
            if rev_gt "$pkg_ver" "$target_version"; then
                echo "Cannot flash $name to $target_version"
                echo "It is not in the packages for $name in firmware-selection-tests"
                exit
            fi
            if rev_eq "$pkg_ver" "$target_version" || \
                    [[ $(jq -r -c '.["upgrade-fence"]' <<< "$pkg") = true ]]; then
                pkg_tgt="$(jq -r -c '.package' <<< "$pkg")"
                break
            fi
        done < <(jq -r -c '.[]' <<< "$pkgs")
    else
        echo "Firmware for $name already at target version $target_version"
        continue
    fi
    if rev_eq "$pkg_ver" "$current_version"; then
        echo "Firmware for $name already at wanted version $pkg_ver"
        continue
    fi
    if [[ ! $pkg_tgt ]]; then
        echo "$name has no package target for $pkg_ver"
        echo "This should never happen"
        exit 1
    fi
    packages_to_run+=("$pkg_tgt")
done < <(jq -r -c '.[]' <<< "${firmware_tests}")

if (( ${#packages_to_run[@]} == 0 )); then
    echo "No firmware update packages need to run"
    exit 0
fi
server="$(get_attr provisioner-webservers |jq -r -c '.[0] | .url')"
firmware_pkgs="$(get_attr firmware-packages)"
for pkgidx in "${packages_to_run[@]}"; do
    pkg="$(jq -r -c ".[\"${pkgidx}\"]" <<< "$firmware_pkgs")"
    if [[ $pkg == null || $pkg == '{}' || ! $pkg ]]; then
        echo "Firmware update package $pkgidx not defined in firmware-packages"
        echo "Fix this and try again."
        exit 1
    fi
    shasum="$(jq -r -c '.sha256sum' <<< "$pkg")"
    source="$(jq -r -c '.source' <<< "$pkg")"
    uri="$server/files/firmware/$pkgidx"
    if ! curl -O -fgl "$uri"; then
        echo "Failed to download $uri"
        echo "Please download the package from:"
        echo "    $source"
        echo "and upload it to the provisioner with:"
        echo "    rebar provisioner files upload $pkgidx to firmware/$pkgidx"
        exit 1
    fi
    if ! sha256sum -c <(printf '%s  %s' "$shasum" "$pkgidx"); then
        echo "Invalid checksum for $pkgidx"
        exit 1
    fi
    printf '%b' "$(jq -r '.script' <<< "$pkg")" > update.sh
    (export package="$pkgidx"; . ./update.sh)
done
