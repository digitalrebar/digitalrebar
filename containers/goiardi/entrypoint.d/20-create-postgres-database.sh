#!/bin/bash

tmpdir=$(mktemp -d /tmp/sqitch-XXXXXX)
cd "$tmpdir"
cp -a "/go/src/github.com/ctdk/goiardi/sql-files/postgres-bundle/"* .

sqitch target add goiardi db:pg://$POSTGRES_USER:$POSTGRES_PASSWORD@database/goiardi
sqitch deploy goiardi

export PGPASSWORD=$POSTGRES_PASSWORD
psql -U $POSTGRES_USER -h database goiardi -c "grant all on database goiardi to goiardi;"
psql -U $POSTGRES_USER -h database goiardi -c "grant all on schema goiardi to goiardi;"
psql -U $POSTGRES_USER -h database goiardi -c "grant all on schema public to goiardi;"
psql -U $POSTGRES_USER -h database goiardi -c "grant all on all tables in schema goiardi to goiardi;"
psql -U $POSTGRES_USER -h database goiardi -c "grant all on all tables in schema public to goiardi;"
psql -U $POSTGRES_USER -h database goiardi -c "grant all on all sequences in schema public to goiardi;"
psql -U $POSTGRES_USER -h database goiardi -c "grant all on all sequences in schema goiardi to goiardi;"
psql -U $POSTGRES_USER -h database goiardi -c "grant all on all functions in schema goiardi to goiardi;"
psql -U $POSTGRES_USER -h database goiardi -c "grant all on all functions in schema public to goiardi;"
cd -
rm -rf "$tmpdir"
