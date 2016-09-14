#!/bin/bash

as_rebar() (
    su -l -c "cd /opt/digitalrebar/core/rails; RAILS_ENV=$RAILS_ENV $*" rebar
)

# Setup database tasks
tasks=("rake db:create"
       "rake railties:install:migrations"
       "rake db:migrate"
       "rake assets:precompile")
for task in "${tasks[@]}"; do
    as_rebar bundle exec $task && continue
    echo "Task $task failed." >&2
    exit 1
done

if ! kv_get digitalrebar/private/api/keys/rebar_key &>/dev/null; then
    as_rebar bundle exec rake db:seed
fi
