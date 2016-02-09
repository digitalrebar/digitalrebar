while [[ ! -e /etc/rebar-data/rebar-key.sh ]] ; do
  sleep 5
done

# Wait for the webserver to be ready.
. /etc/rebar-data/rebar-key.sh
while ! rebar ping &>/dev/null; do
  sleep 1
  . /etc/rebar-data/rebar-key.sh
done

# Wait for the $SERVICE_DEPLOYMENT deployment to show up

while ! rebar deployments show $SERVICE_DEPLOYMENT &>/dev/null; do
    sleep 1
done
