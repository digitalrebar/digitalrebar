#!/bin/bash

rebar nodes commit "$HOSTNAME" || :
rebar nodes update "$HOSTNAME" "{\"alive\": true, \"bootenv\": \"local\"}"

