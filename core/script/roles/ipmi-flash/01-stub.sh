#!/usr/bin/env bash

declare -A PARAMS
PARAMS['ipmi-firmware-rev']='rebar_wall/ipmi/bmcinfo/firmware_rev'
PARAMS['ipmi-mfgr-id']='rebar_wall/ipmi/bmcinfo/mfgr_id'
PARAMS['ipmi-product-id']='rebar_wall/ipmi/bmcinfo/product_id'
PARAMS['ipmi-device-id']='rebar_wall/ipmi/bmcinfo/device_id'
PARAMS['ipmi-device-rev']='rebar_wall/ipmi/bmcinfo/device_rev'

wanted_version=$(read_attribute "ipmi/wanted-version")

rev_lt() {
    # $1 = current firmware rev
    # $2 = target firmware rev
    local val_a=() val_b=()
    IFS='.-_' read -a val_a <<< "$1"
    IFS='.-_' read -a val_b <<< "$2"
    for ((i=0; i<${#val_a[@]}; i++)); do
         (( ${val_a[$i]} == ${val_b[$i]} )) || return $(( ${val_a[$i]} >= ${val_b[$i]} ))
    done
    # One day handle dissimilar array lengths
    return 1
}

skip_flash="$(read_attribute 'rebar/skip_flash')"
if [[ $skip_flash = true ]]; then
    echo "Skipping IPMI flash due to skip_flash being true"
    exit 0
fi

declare -A VALUES

for v in "${!PARAMS[@]}"; do
    VALUES["$v"]="$(read_attribute "${PARAMS[$v]}")"
done

packages=()
entries=()
versions=()
applicable=()
forcing=()

while read -r entry; do
    found=true
    isapplicable=true
    forceit=false
    package="$(jq -r -c '.["package"]' <<< "$entry")"
    printf "Testing firmware update %s\n" "$package"
    for v in "${!PARAMS[@]}"; do
        val=$(jq -r -c ".[\"$v\"]" <<< "$entry")
        [[ ! $val || $val = null ]] && continue
        case $v in
            'ipmi-firmware-rev')
                printf "Testing firmware version '%s' against '%s'\n" "${VALUES[$v]}" "$val"
                version="$val"

                if [[ "$wanted_version" == "latest" ]] ; then
                    if ! rev_lt "${VALUES[$v]}" "$version"; then
                        isapplicable=false
                    fi
                else
                    if [[ "$wanted_version" == "$version" ]] ; then
                        if [[ "${VALUES[$v]}" == "$version" ]] ; then
                            # already at the desired version
                            isapplicable=false
                        else
                            # Else - we need to apply the package.
                            isapplicable=true
                            # if we have to go backwards force it
                            if ! rev_lt "${VALUES[$v]}" "$version"; then
                                forceit=true
                            fi
                        fi
                    else
                        # Isn't the desired version - skip it.
                        isapplicable=false
                        # We could test less than and walk up to wanted.  ???
                    fi
                fi;;
            *)
                printf "Checking that '%s' value '%s' = '%s'\n" "$v" "$val" "${VALUES[$v]}"
                if [[ $val != ${VALUES[$v]} ]] ; then
                    found=false
                    break
                fi
        esac
    done
    if [[ $found = true ]]; then
        versions+=("$version")
        packages+=("$package")
        applicable+=($isapplicable)
        forcing+=(forceit)
        entries+=("$entry")
        if [[ $isapplicable = true ]]; then
            printf 'Package %s is valid for this system\n' "$package"
        else
            printf 'Package %s is valid for this system, but old or current.  Keep looking ...\n' "$package"
        fi
    else
        printf 'Package %s is not valid for this system\n' "$package"
    fi
done < <(jq -r -c '.[]' <<< "$(read_attribute 'ipmi/firmware_updates')")

if [[ ${#packages[@]} = 0 ]]; then
    echo "No IPMI firmware update found for this system"
    exit 0
fi

isapplicable=false
package=""
entry=""
matchwanted=false
if [[ $wanted_version == ${VALUES['ipmi-firmware-rev']} ]] ; then
    matchwanted=true
fi

json="{"
COMMA=""
for index in "${!packages[@]}"
do
    # Record first appliable package to apply
    if [[ $isapplicable == false && ${applicable[$index]} == true ]] ; then
        package=${packages[$index]}
        entry=${entries[$index]}
        isapplicable=true
        forceit=${forcing[$index]}
    fi
    if [[ ${versions[$index]} == ${wanted_version} || $wanted_version == "latest" ]] ; then
        matchwanted=true
    fi
    json="$json$COMMA { \"package\": \"${packages[$index]}\", \"verison\": \"${versions[$index]}\", \"applicable\": ${applicable[$index]} }"
    COMMA=","
done
json="$json }"
write_attribute 'ipmi/firmware_applicable' "$json"

if [[ $isapplicable = false && $matchwanted = false ]] ; then
    echo "IPMI firmware can't update to wanted version"
    exit -1
fi

if [[ $isapplicable = false ]]; then
    echo "IPMI firmware already up to date"
    exit 0
fi

# Entry points at the firmware entry to update.  Download package from
# the provisioner, check its sha256sum, and run its script if all
# appears sane.

server="$(read_attribute 'rebar/provisioner/server/webservers' |jq -r -c '.[0] | .url')"
if ! curl -O -fgl "$server/files/ipmi/$package"; then
    echo "Failed to download $package from $server/files/ipmi"
    echo "Make sure it has been made available."
    exit 1
fi
if ! sha256sum -c <(printf '%s  %s' "$(jq -r '.["package-sha256sum"]' <<< "$entry")" "$package"); then
    echo "Invalid checksum for $package"
    exit 1
fi
history=$(read_attribute 'ipmi/firmware_history')
history="$history$(date) $version $package\n"
write_attribute 'ipmi/firmware_history' "$history"
printf '%b' "$(jq -r '.script' <<< "$entry")" > update.sh
. ./update.sh
