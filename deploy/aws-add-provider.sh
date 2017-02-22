#!/usr/bin/env bash

#
# aws-add-provider
#

start_args="$@"

args=()
while (( $# > 0 )); do
    arg="$1"
    arg_key="${arg%%=*}"
    arg_data="${arg#*=}"
    case $arg_key in
        # This used to process init-files.sh and workload.sh args
        --*)
            arg_key="${arg_key#--}"
            arg_key="${arg_key//-/_}"
            arg_key="${arg_key^^}"
            echo "Overriding $arg_key with $arg_data"
            export $arg_key="$arg_data"
            ;;
        *)
            args+=("$arg");;
    esac
    shift
done

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
	echo "PROVIDER: ${PROVIDER} exists as ID ${PROVIDER_ID}. not added"
	echo "PROVIDER: To remove: rebar providers destroy $PROVIDER_ID"
else
	echo "PROVIDER: creating ${PROVIDER}-provider from ~/.aws/* files"

    provider="{
	  \"name\": \"$PROVIDER\",
	  \"description\": \"AWS Provider\",
	  \"type\": \"AwsProvider\",
	  \"auth_details\": {
	    \"provider\": \"AWS\",
	    \"aws_access_key_id\": \"$PROVIDER_AWS_ACCESS_KEY_ID\",
	    \"aws_secret_access_key\": \"$PROVIDER_AWS_SECRET_ACCESS_KEY\",
	    \"region\": \"$PROVIDER_AWS_REGION\"
	  }
	}"

    PROVIDER_ID=$(rebar providers create "$provider" | jq .id)
    echo "PROVIDER: created with ID $PROVIDER_ID"
fi


