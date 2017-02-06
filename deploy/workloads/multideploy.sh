#!/usr/bin/env bash

#
# workloads/multideploy.sh
#

start_args="$@"

if ! which rebar &>/dev/null; then
	echo "Missing Rebar CLI, see http://digital-rebar.readthedocs.io/en/latest/clients/cli.html"
	break
fi

if ! which jq &>/dev/null; then
	echo "Must have JQ"
	break
fi

#
# Process config and validate providers
#
. workloads/wl-lib.sh

if [[ ! $ADMIN_IP ]] ; then
    echo "Must specify --admin-ip"
    exit 1
fi

for file in workloads/cluster/deploy-*.json
do
	echo "============= READING FILE ${file} ===================="

	PROVIDER=$(cat ${file} | jq .provider.name | sed -r 's/\"(.*)-provider"/\1/g')
	echo "  PROVIDER: creating ${PROVIDER}"
	PROVIDER_ID=$(rebar providers match "{\"name\":\"${PROVIDER}-provider\"}" | jq 'map(.id)[0]')
	if [[ $PROVIDER_ID -gt 0 ]] ; then
		echo "  PROVIDER: ${PROVIDER} exists as ID ${PROVIDER_ID}. not added"
	else
		echo "  PROVIDER: creating ${PROVIDER} from ~/.dr_info file"
		validate_provider $PROVIDER
		add_provider
	fi

	TENANT=$(cat ${file} | jq .tenant | sed -r 's/\"(.*)\"/\1/g')
	if [[ TENANT ]] ; then 
		echo "  TENANT: Deployment file contains TENANT field"
		TENANT_ID=$(rebar tenants match "{\"name\":\"${TENANT}\"}" | jq 'map(.id)[0]')
		if [[ $TENANT_ID -gt 0 ]] ; then
			echo "  TENANT: ${TENANT} exists as ID $TENANT_ID. not added"
		else
			echo "  TENANT: Creating ${TENANT}"
			TENANT_ID=$(rebar tenants create "{\"name\":\"${TENANT}\", \"parent_id\":1, \"description\":\"added by multicluster script\"}" | jq .id)
			# remove automatic deployment
			rebar deployments destroy "tenant-${TENANT}-root"
		fi
		echo "  TENANT: Using ID ${TENANT_ID}"
	fi

	DEPLOYMENT_NAME=$(cat ${file} | jq .name | sed -r 's/\"(.*)\"/\1/g')
	if [[ ! $DESTROY ]] ; then
		echo "  DEPLOYMENT: creating ${DEPLOYMENT_NAME}"
		JSON=$(cat ${file})
		if [[ TENANT_ID ]] ; then
			echo "  DEPLOYMENT: Replace Tenant ${TENANT} with ID ${TENANT_ID}"
			JSON="${JSON/\"tenant\": \"$TENANT\"/\"tenant_id\": $TENANT_ID}"
		fi
		DEPLOYMENT_ID=$(rebar deployments create "${JSON}" | jq .id)
	else
		DEPLOYMENT_ID=$(rebar deployments show ${DEPLOYMENT_NAME} | jq .id)
		if [[ $DEPLOYMENT_ID ]] ; then

			echo "  DEPLOYMENT: collecting nodes for ID ${DEPLOYMENT_ID}"
			declare -a NODES=$(rebar nodes match "{\"deployment_id\":${DEPLOYMENT_ID}}" | jq 'map(.id)[]')

			echo "  DEPLOYMENT: destroy ID ${DEPLOYMENT_ID}: ${DEPLOYMENT_NAME}"
			rebar deployments destroy "${DEPLOYMENT_NAME}"

			if [[ NODES ]] ; then
				for n in ${NODES[@]}
				do
					echo "  DEPLOYMENT: destroying node ID ${n}"
					rebar nodes destroy $n
				done
			fi
		else
			echo "  DEPLOYMENT ${DEPLOYMENT_NAME} does NOT exist"
		fi
	fi
done