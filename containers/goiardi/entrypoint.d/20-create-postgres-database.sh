#!/bin/bash

tmpdir=$(mktemp -d /tmp/sqitch-XXXXXX)
cd "$tmpdir"
cp -a /tmp/postgres-bundle/* .
export PGPASSWORD=$POSTGRES_PASSWORD
addr="$(curl http://localhost:8500/v1/catalog/service/rebar-database |jq -r '.[0].Address')"
while ! psql -U $POSTGRES_USER -h "$addr" goiardi -c 'select 1;'; do
    addr="$(curl http://localhost:8500/v1/catalog/service/rebar-database |jq -r '.[0].Address')"
    sleep 1
done

sqitch target add goiardi db:pg://$POSTGRES_USER:$POSTGRES_PASSWORD@"$addr"/goiardi
sqitch verify goiardi || sqitch deploy goiardi

psql -U $POSTGRES_USER -h "$addr" goiardi -c "grant all on database goiardi to goiardi;"
psql -U $POSTGRES_USER -h "$addr" goiardi -c "grant all on schema goiardi to goiardi;"
psql -U $POSTGRES_USER -h "$addr" goiardi -c "grant all on schema public to goiardi;"
psql -U $POSTGRES_USER -h "$addr" goiardi -c "grant all on all tables in schema goiardi to goiardi;"
psql -U $POSTGRES_USER -h "$addr" goiardi -c "grant all on all tables in schema public to goiardi;"
psql -U $POSTGRES_USER -h "$addr" goiardi -c "grant all on all sequences in schema public to goiardi;"
psql -U $POSTGRES_USER -h "$addr" goiardi -c "grant all on all sequences in schema goiardi to goiardi;"
psql -U $POSTGRES_USER -h "$addr" goiardi -c "grant all on all functions in schema goiardi to goiardi;"
psql -U $POSTGRES_USER -h "$addr" goiardi -c "grant all on all functions in schema public to goiardi;"
cd -
rm -rf "$tmpdir"
mkdir -p /etc/goiardi/
sed -i "s/::database::/$addr/" /etc/goiardi/goiardi.conf
