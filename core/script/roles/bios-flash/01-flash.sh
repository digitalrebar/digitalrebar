#!/usr/bin/env bash

declare -A PARAMS
PARAMS['bios-vendor']='ohai/dmi/bios/vendor'
PARAMS['bios-version']='ohai/dmi/bios/version'
PARAMS['bios-revision']='ohai/dmi/bios/bios_revision'
PARAMS['system_manufacturer']='ohai/dmi/system/manufacturer'
PARAMS['system_product']='ohai/dmi/system/product_name'
PARAMS['baseboard_manufacturer']='ohai/dmi/base_board/manufacturer'
PARAMS['baseboard_product_name']='ohai/dmi/base_board/product_name'

rev_lt() {
    # $1 = current firmware rev
    # $2 = target firmware rev
    local val_a=() val_b=()
    IFS='.-_' read -a val_a <<< "$1"
    IFS='.-_' read -a val_b <<< "$2"
    for ((i=0; i<${#val_a[@]}; i++)); do
         [[ ${val_a[$i]} < ${val_b[$i]} ]] && return 0
    done
    return 1
}

declare -A VALUES

for v in "${!PARAMS[@]}"; do
    VALUES["$v"]="$(read_attribute "${PARAMS[$v]}")"
done

while read -r entry; do
    match=true
    found=true
    package="$(jq -r -c '.["package"]' <<< "$entry")"
    printf "Testing firmware update %s\n" "$package"
    for v in "${!PARAMS[@]}"; do
        val=$(jq -r -c ".[\"$v\"]" <<< "$entry")
        [[ ! $val || $val = null ]] && continue
        case $v in
            bios-version|bios-revision)
                printf "Testing firmware version '%s' against '%s'\n" "$v" "$val"
                version=$val
                if ! rev_lt "${VALUES[$v]}" "$val"; then
                    match=false
                fi;;
            *)
                printf "Checking that '%s' value '%s' = '%s'\n" "$v" "$val" "${VALUES[$v]}"
                if [[ $val != ${VALUES[$v]} ]]; then
                    found=false
                    break
                fi
        esac
    done
    if [[ $found = true ]]; then
        printf 'Package %s is valid for this system\n' "$package"
        break
    else
        printf 'Package %s is not valid for this system\n' "$package"
    fi
done < <(jq -r -c '.[]' <<< "$(read_attribute 'bios/firmware_updates')")
if [[ $found = false ]]; then
    echo "No BIOS update found for this system"
    exit 0
fi
write_attribute 'bios/firmware_applicable_version' "$version"
write_attribute 'bios/firmware_applicable_package' "$package"
if [[ $match = false ]]; then
    write_attribute 'bios/firmware_updated' false
    echo "BIOS $package already up to date"
    exit 0
fi

# Entry points at the firmware entry to update.  Download package from
# the provisioner, check its sha256sum, and run its script if all
# appears sane.

server="$(read_attribute 'rebar/provisioner/server/webservers' |jq -r -c '.[0] | .url')"
if ! curl -O -fgl "$server/files/bios/$package"; then
    echo "Failed to download $package from $server/files/bios"
    echo "Make sure it has been made available."
    exit 1
fi
if ! sha256sum -c <(printf '%s  %s' "$(jq -r '.["package-sha256sum"]' <<< "$entry")" "$package"); then
    echo "Invalid checksum for $package"
    exit 1
fi

write_attribute 'bios/firmware_updated' true
printf '%b' "$(jq -r '.script' <<< "$entry")" > update.sh
. ./update.sh
