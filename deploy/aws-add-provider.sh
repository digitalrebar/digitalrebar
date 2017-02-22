#!/usr/bin/env bash

#
# aws-add-provider
#

start_args="$@"
. workloads/wl-lib.sh

if ! which rebar &>/dev/null; then
	echo "Missing Rebar CLI, see http://digital-rebar.readthedocs.io/en/latest/clients/cli.html"
	exit 1
fi

if ! which jq &>/dev/null; then
	echo "Must have JQ"
	exit 1
fi

if [[ -e ~/.aws/credentials ]]; then
	KPROVIDER_AWS_ACCESS_KEY_IDEY=$(cat ~/.aws/credentials | grep aws_access_key_id | awk -F" = " '{print $2}')
	PROVIDER_AWS_SECRET_ACCESS_KEY=$(cat ~/.aws/credentials | grep aws_secret_access_key | awk -F" = " '{print $2}')
else
	echo "You must have an AWS credentials file, run the aws client!"
	exit 1
fi

if [[ -f ~/.aws/config ]]; then
	PROVIDER_AWS_REGION=$(cat ~/.aws/config | grep region | awk -F" = " '{print $2}')
else
	echo "You must have an AWS config file, run the aws client!"
	exit 1
fi

if [[ ! $ADMIN_IP ]] ; then
    echo "Must specify --admin-ip=[address]"
    exit 1
fi

PROVIDER=${PROVIDER:-"aws"}
PROVIDER_ID=$(rebar providers match "{\"name\":\"${PROVIDER}-provider\"}" | jq 'map(.id)[0]')
if [[ $PROVIDER_ID -gt 0 ]] ; then
	echo "  PROVIDER: ${PROVIDER} exists as ID ${PROVIDER_ID}. not added"
else
	echo "  PROVIDER: creating ${PROVIDER}-provider from ~/.aws/* files"
	validate_provider $PROVIDER
	add_provider
fi


