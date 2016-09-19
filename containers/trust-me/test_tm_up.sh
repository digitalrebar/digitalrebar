#!/bin/bash

curl -X POST -k  -d '{"label": "internal"}' https://localhost:8888/api/v1/cfssl/info

