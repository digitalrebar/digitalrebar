

if [[ ! -f /var/cache/trust_me/ca.pem ]] ; then
   cd /var/cache/trust_me
   cfssl gencert -initca /etc/trust_me/csr_ca.json | cfssljson -bare ca
   cd -
fi

