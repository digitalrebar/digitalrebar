rebar_data=''
while true; do
    if ! rebar_data=$(kv_get digitalrebar/private/api/keys/rebar_key); then
        sleep 5
        continue
    fi
    cat >/etc/rebar-data/rebar-key.sh <<< "$rebar_data"
    . /etc/rebar-data/rebar-key.sh
    if rebar deployments show $SERVICE_DEPLOYMENT &>/dev/null; then
        break
    fi
    sleep 1
done
