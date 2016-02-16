#!/bin/bash

make_service proxy 3128 '{"script": "pidof squid3","interval": "10s"}'
consul reload

