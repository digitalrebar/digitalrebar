#!/bin/bash

# Make sure we set the token type
set_service_attrib dhcp-service dhcp_servers "{\"value\": [\"${EXTERNAL_IP%%/*}\"]}"

