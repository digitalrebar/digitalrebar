set -x
set -e

# Set up area protection
echo '{
  "ID": "anonymous",
  "Type": "client",
  "Rules": "key \\"digitalrebar/private\\" { policy = \\"deny\\" }"
}' > /tmp/tmp_cred.json
# GREG: THIS ISN"T WORKING CORRECTLY
curl -X PUT -d @/tmp/tmp_cred.json http://localhost:8500/v1/acl/update?token=$CONSUL_M_ACL
rm -f /tmp/tmp_cred.json

rebar_password=fred # GREG: SecureRandom.base64.gsub('=','3')
rebar_user=rebar
rebar_database=digitalrebar

gosu postgres createuser -d -E -S -R -w ${rebar_user}
gosu postgres psql -c "ALTER USER ${rebar_user} WITH ENCRYPTED PASSWORD '${rebar_password}';"

baseurl="http://127.0.0.1:8500/v1/kv/digitalrebar/private/database/digitalrebar"
token="?token=$CONSUL_M_ACL"

curl --data-binary "${rebar_database}" -X PUT ${baseurl}/database${token}
curl --data-binary "${rebar_user}" -X PUT ${baseurl}/username${token}
curl --data-binary "${rebar_password}" -X PUT ${baseurl}/password${token}

