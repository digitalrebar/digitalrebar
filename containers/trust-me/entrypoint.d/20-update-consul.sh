#!/bin/bash


make_service "trust-me" "8888" '{"script": "/usr/local/bin/test_tm_up.sh","interval": "10s"}'

