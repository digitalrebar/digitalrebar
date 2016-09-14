#!/bin/bash
rm -f ~/.pgpass
pass=''
while [[ ! $pass ]]; do
    pass=$(kv_get digitalrebar/private/database/goiardi/password) || :
    [[ $pass ]] || sleep 5
done
echo $pass >~/.pgpass
chmod 600 ~/.pgpass
