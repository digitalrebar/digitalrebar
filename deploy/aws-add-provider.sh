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


if [[ ! $ADMIN_IP ]] ; then
    echo "Must specify --admin-ip=[address]"
    exit 1
fi

if [[ $CREDENTIALS ]] ; then
    echo "Using provided credentials"
else
	CREDENTIALS="rebar:rebar1"
    echo "Using default credentials, override using --credentials=USER:PASS"
fi


if ! which jq &>/dev/null; then
	echo "Must have JQ"
	exit 1
fi

if ! which aws &>/dev/null; then
	echo "Must have aws client installed"
	exit 1
fi

if [[ -e ~/.aws/credentials ]]; then
	PROVIDER_AWS_ACCESS_KEY_ID=$(cat ~/.aws/credentials | grep aws_access_key_id | awk -F" = " '{print $2}')
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

PROVIDER=${PROVIDER:-"aws"}
if which rebar &>/dev/null; then
	PROVIDER_ID=$(rebar -E "https://$ADMIN_IP" providers match "{\"name\":\"${PROVIDER}-provider\"}" | jq 'map(.id)[0]')
else
	echo "Using CURL.  You can also install the Rebar CLI, see http://digital-rebar.readthedocs.io/en/latest/clients/cli.html"
fi


JSON="{
  \"name\": \"${PROVIDER}-provider\",
  \"description\": \"AWS Provider\",
  \"type\": \"AwsProvider\",
  \"auth_details\": {
    \"provider\": \"AWS\",
    \"aws_access_key_id\": \"$PROVIDER_AWS_ACCESS_KEY_ID\",
    \"aws_secret_access_key\": \"$PROVIDER_AWS_SECRET_ACCESS_KEY\",
    \"region\": \"$PROVIDER_AWS_REGION\"
  }
}"


if [[ $PROVIDER_ID -gt 0 ]] ; then
	echo "PROVIDER: ${PROVIDER} exists as ID ${PROVIDER_ID}. not added"
	echo "PROVIDER: To remove: rebar providers destroy $PROVIDER_ID"
else
	echo "PROVIDER: creating ${PROVIDER}-provider from ~/.aws/* files"
	PROVIDER_ID=$(curl --insecure --digest -u "${CREDENTIALS}" -X POST -H "Accept: application/json" -H "Content-Type: application/json" -d "${JSON}" "https://$ADMIN_IP/api/v2/providers" | jq .id)
    echo "PROVIDER: created ${PROVIDER}-provider with ID $PROVIDER_ID"
fi