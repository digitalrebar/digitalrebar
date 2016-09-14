# deploy-rebar-api

To create a PEM file from the generated cert, use the following: `docker exec -it compose_rebar_api_1 openssl rsa -outform PEM -in /var/run/rebar/server.key`
