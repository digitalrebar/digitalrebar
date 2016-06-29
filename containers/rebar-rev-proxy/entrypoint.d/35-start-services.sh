#!/bin/bash

if [[ $REVPROXY_AUTHFILTER ]] ; then
	AUTHFILTER="-authFilter $REVPROXY_AUTHFILTER"
fi

if [[ $REVPROXY_PORT ]] ; then
	LISTENPORT="-listenPort $REVPROXY_PORT"
else
	REVPROXY_PORT=443
	LISTENPORT="-listenPort $REVPROXY_PORT"
fi

if [[ $REVPROXY_REALM ]] ; then
	REALM="-digestRealm $REVPROXY_REALM"
fi

if [[ $REVPROXY_DB_STORE_TYPE ]] ; then
	DBSTORETYPE="-dbStoreType $REVPROXY_DB_STORE_TYPE"
fi

if [[ $REVPROXY_DB_PATH ]] ; then
	DBPATH="-dbFilename $REVPROXY_DB_PATH"
fi

if [[ $REVPROXY_CONSUL_KEY ]] ; then
	CONSULKEY="-dbConsulKey $REVPROXY_CONSUL_KEY"
fi

if [[ $REVPROXY_SAML_IDP_CERT_FILE ]] ; then
	SAMLCERT="-samlIdpcert $REVPROXY_SAML_IDP_CERT_FILE"
fi

if [[ $REVPROXY_SAML_IDP_SSO_URL ]] ; then
	SAMLIDPSSOURL="-samlIdpssourl $REVPROXY_SAML_IDP_SSO_URL"
fi

if [[ $REVPROXY_SAML_IDP_SSO_DESC_URL ]] ; then
	SAMLIDPSSODESCURL="-samlIdpssodescurl $REVPROXY_SAML_IDP_SSO_DESC_URL"
fi

if [[ $FORWARDER_IP && $forwarder ]]; then
  FORWARDER_FLAG="-forwarder"
fi

run_forever() (
  while true; do
    "$@"
    sleep 5
  done
)

run_forever /usr/local/bin/rebar-rev-proxy --externalIp ${EXTERNAL_IP%%/*} \
	$FORWARDER_FLAG \
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

