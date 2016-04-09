#!/bin/bash

if [[ $REVPROXY_AUTHFILTER ]] ; then
	AUTHFILTER="-auth_filter $REVPROXY_AUTHFILTER"
fi

if [[ $REVPROXY_PORT ]] ; then
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


/usr/local/bin/rebar-rev-proxy $LISTENPORT $AUTHFILTER $REALM $DBSTORETYPE $DBPATH $CONSULKEY &

