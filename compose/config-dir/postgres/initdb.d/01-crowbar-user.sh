set -x
set -e

# Set up area protection
echo '{
  "ID": "anonymous",
  "Type": "client",
  "Rules": "key \\"opencrowbar/private\\" { policy = \\"deny\\" }"
}' > /tmp/tmp_cred.json
# GREG: THIS ISN"T WORKING CORRECTLY
curl -X PUT -d @/tmp/tmp_cred.json http://localhost:8500/v1/acl/update?token=$CONSUL_M_ACL
rm -f /tmp/tmp_cred.json

crowbar_password=fred # GREG: SecureRandom.base64.gsub('=','3')
crowbar_user=crowbar
crowbar_database=opencrowbar

gosu postgres createuser -d -E -S -R -w ${crowbar_user}
gosu postgres psql -c "ALTER USER ${crowbar_user} WITH ENCRYPTED PASSWORD '${crowbar_password}';"

baseurl="http://127.0.0.1:8500/v1/kv/opencrowbar/private/database/opencrowbar"
token="?token=$CONSUL_M_ACL"

curl --data-binary "${crowbar_database}" -X PUT ${baseurl}/database${token}
curl --data-binary "${crowbar_user}" -X PUT ${baseurl}/username${token}
curl --data-binary "${crowbar_password}" -X PUT ${baseurl}/password${token}

