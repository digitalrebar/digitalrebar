# Wait for key with certificate
ca_string=$(kv_get "trust_me/certificate")
while [[ $ca_string == "" ]] ; do
    sleep 1
    ca_string=$(kv_get "trust_me/certificate")
done
mkdir -p /etc/rule-engine
echo "$ca_string" > /etc/rule-engine/cacert.pem

generate_crt "rule-engine" "rule-engine" "rule-engine,rule-engine-servce,$IP,${EXTERNAL_IP%%/*},${HOSTNAME},127.0.0.1,localhost"

mv rule-engine.pem /etc/rule-engine/cert.pem
mv rule-engine.csr /etc/rule-engine/csr.json
mv rule-engine-key.pem /etc/rule-engine/key.pem

chmod 600 /etc/rule-engine/*
