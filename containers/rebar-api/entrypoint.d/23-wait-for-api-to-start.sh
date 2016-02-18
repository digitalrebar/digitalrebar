#!/bin/bash

while ! rebar -U rebar -P rebar1 ping; do
    sleep 5
done

make_service rebar-api 3000 '{"script": "rebar -E $REBAR_ENDPOINT -U rebar -P rebar1 ping","interval": "10s"}'
