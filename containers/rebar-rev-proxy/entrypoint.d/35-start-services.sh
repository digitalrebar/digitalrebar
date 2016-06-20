#!/bin/bash

if [[ $REVPROXY_AUTHFILTER ]] ; then
	AUTHFILTER="-auth_filter $REVPROXY_AUTHFILTER"
fi

if [[ $REVPROXY_PORT ]] ; then
	LISTENPORT="-listen_port $REVPROXY_PORT"
else
	REVPROXY_PORT=443
	LISTENPORT="-listen_port $REVPROXY_PORT"
fi

if [[ $REVPROXY_REALM ]] ; then
	REALM="-digest_realm $REVPROXY_REALM"
fi

if [[ $REVPROXY_DB_STORE_TYPE ]] ; then
	DBSTORETYPE="-db_store_type $REVPROXY_DB_STORE_TYPE"
fi

if [[ $REVPROXY_DB_PATH ]] ; then
	DBPATH="-db_file_name $REVPROXY_DB_PATH"
fi

if [[ $REVPROXY_CONSUL_KEY ]] ; then
	CONSULKEY="-db_consul_key $REVPROXY_CONSUL_KEY"
fi

if [[ $REVPROXY_SAML_IDP_CERT_FILE ]] ; then
	SAMLCERT="-saml_idpcert $REVPROXY_SAML_IDP_CERT_FILE"
fi

if [[ $REVPROXY_SAML_IDP_SSO_URL ]] ; then
	SAMLIDPSSOURL="-saml_idpssourl $REVPROXY_SAML_IDP_SSO_URL"
fi

if [[ $REVPROXY_SAML_IDP_SSO_DESC_URL ]] ; then
	SAMLIDPSSODESCURL="-saml_idpssodescurl $REVPROXY_SAML_IDP_SSO_DESC_URL"
fi

if [[ $FORWARDER_IP && $forwarder ]]; then
  FORWARDER_FLAG="-forwarder"
fi

/usr/local/bin/rebar-rev-proxy --external_ip ${EXTERNAL_IP%%/*} \
  $FORWARDER \
	$LISTENPORT \
	$AUTHFILTER \
	$REALM \
	$DBSTORETYPE \
	$DBPATH \
	$SAMLCERT \
	$SAMLIDPSSOURL \
	$SAMLIDPSSODESCURL \
	$CONSULKEY &

make_service "rebar-rev-proxy" $REVPROXY_PORT '{"script": "pidof rebar-rev-proxy", "interval": "10s"}'

