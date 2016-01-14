#!/bin/bash
pass=''
while [[ ! $pass ]] ; do
    pass=$(kv_get digitalrebar/private/database/digitalrebar/password) || :
    [[ $pass ]] || sleep 5
done
echo $pass >~/.pgpass
chmod 600 ~/.pgpass

cat > /etc/consul_m_acl.json <<EOF
{
  "acl_master_token": "${CONSUL_M_ACL}"
}
EOF
chown rebar:rebar /etc/consul_m_acl.json
