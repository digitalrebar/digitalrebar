#!/bin/bash

# Make sure the key file is there
mkdir -p /root/.ssh
NEW_FILE="/tmp/tt.$$.ca"
AUTH_FILE="/root/.ssh/authorized_keys"

# Put keys in place
key_names=$(get_keys "rebar/access_keys")
touch "${NEW_FILE}"
if [[ "${key_names}" != "" ]]; then
    for name in $key_names
    do
      read_attribute "rebar/access_keys/${name}" >> "${NEW_FILE}"
      echo >> "${NEW_FILE}"
    done
fi
sort -u "${NEW_FILE}" > "${NEW_FILE}.2"

lead='^### BEGIN GENERATED CONTENT$'
tail='^### END GENERATED CONTENT$'

if ! grep -q "$lead" "${AUTH_FILE}" ; then
  echo "### BEGIN GENERATED CONTENT" >> "${AUTH_FILE}"
  echo "### END GENERATED CONTENT" >> "${AUTH_FILE}"
fi

sed -e "/$lead/,/$tail/{ /$lead/{p; r ${NEW_FILE}.2
        }; /$tail/p; d }" "$AUTH_FILE" > "${NEW_FILE}.3"

# Make the file contents uniq and only update if needed
if ! diff -q "${NEW_FILE}.3" "${AUTH_FILE}" 2>&1 >/dev/null; then
    cp "${NEW_FILE}.3" "${AUTH_FILE}"
    chmod 600 "${AUTH_FILE}"
fi
rm -f "${NEW_FILE}" "${NEW_FILE}.2" "${NEW_FILE}.3"

# Put machine key in place if changed or not present
NEW_FILE="/tmp/tt.$$.ca"
KEY_FILE="/etc/rebar.install.key"
read_attribute_file_content "rebar/machine_key" "${NEW_FILE}"
if ! diff -q "${NEW_FILE}" "${KEY_FILE}" 2>&1 >/dev/null; then
    cp "${NEW_FILE}" "${KEY_FILE}"
    chmod 600 "${KEY_FILE}"
fi
rm -f "${NEW_FILE}"

