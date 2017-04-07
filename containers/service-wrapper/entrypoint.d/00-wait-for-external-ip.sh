set +e
# Wait for external IP to show up in this container
count=0
while ! (ip -4 -o addr show | grep -q "${EXTERNAL_IP}"); do
    sleep 1
    if [ $((count % 60)) -eq 0 ] ; then
        echo "Waiting for $EXTERNAL_IP"
    fi
    count=$((count+1))
done
set -e
