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

JSON="{
  \"name\": \"aws-provider\",
  \"description\": \"Quickstart AWS Provider\",
  \"type\": \"AwsProvider\",
  \"auth_details\": {
    \"provider\": \"AWS\",
    \"aws_access_key_id\": \"$PROVIDER_AWS_ACCESS_KEY_ID\",
    \"aws_secret_access_key\": \"$PROVIDER_AWS_SECRET_ACCESS_KEY\",
    \"region\": \"$PROVIDER_AWS_REGION\"
  }
}"

# check to see if provider already exists
PROVIDER=$(curl -sL -o /dev/null -w "%{http_code}" --insecure --digest -u "${CREDENTIALS}" -X GET "https://$ADMIN_IP/api/v2/providers/aws-provider")

if [[ $PROVIDER -eq "200" ]] ; then
	echo "PROVIDER: aws-provider exists. not added"
else
	echo "PROVIDER: creating aws-provider from ~/.aws/* files"
	curl --insecure --digest -u "${CREDENTIALS}" -X POST -H "Accept: application/json" -H "Content-Type: application/json" -d "${JSON}" "https://$ADMIN_IP/api/v2/providers"

    # check to see if provider already exists
    PROVIDER=$(curl -sL -o /dev/null -w "%{http_code}" --insecure --digest -u "${CREDENTIALS}" -X GET "https://$ADMIN_IP/api/v2/providers/aws-provider")

    if [[ $PROVIDER -eq "200" ]] ; then
        echo "PROVIDER: aws-provider created"
    else
        echo "PROVIDER: error $PROVIDER creating provider"
    fi
    
fi


