set -e

password=fred # GREG: SecureRandom.base64.gsub('=','3')
user=goiardi
database=goiardi

su -c "createuser -S ${user} " postgres
gosu postgres psql -c "ALTER USER ${user} WITH ENCRYPTED PASSWORD '${password}';"
su -c "createdb ${database} -O ${user}" postgres

baseurl="http://127.0.0.1:8500/v1/kv/digitalrebar/private/database/${database}"
token="?token=$CONSUL_M_ACL"

curl --data-binary "${database}" -X PUT ${baseurl}/database${token}
curl --data-binary "${user}" -X PUT ${baseurl}/username${token}
curl --data-binary "${password}" -X PUT ${baseurl}/password${token}

