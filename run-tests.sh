#!/bin/bash
# Actually run the BDD, rake, and rspec tests.
# This assumes that the proper environment has been set up.
set +e
run_crowbar() (
    export CROWBAR_KEY='crowbar:crowbar'
    echo "$CROWBAR_KEY" >/etc/crowbar.install.key
    cd /opt/opencrowbar/core/rails
    tasks=("rake db:drop"
        "rake db:create"
        "rake railties:install:migrations"
        "rake db:migrate"
        "rake db:seed")
    for task in "${tasks[@]}"; do
        bundle exec $task
    done
    QUE_WORKER_COUNT=2 QUE_QUEUE=HighPriorityRunner bundle exec rake que:work &
    QUE_WORKER_COUNT=10 QUE_QUEUE=NodeRoleRunner bundle exec rake que:work &
    bundle exec rails server -d
    while ! crowbar ping; do
        sleep 1
    done
    (cd ..; CROWBAR_KEY="crowbar:crowbar" bin/barclamp_import $PWD)
)

kill_crowbar() (
    pkill -f rails
    pkill -f que:work
    cd /opt/opencrowbar/core/rails
    bundle exec rake db:drop
)

test_bdd() (
    export RAILS_ENV=development
    run_crowbar
    cd /opt/opencrowbar/core/BDD 
    erlc +debug_info *.erl
    erl -s bdd test travis -s init stop -noshell
    local res=$?
    kill_crowbar
    return $res
)

test_rake() (
    export RAILS_ENV=test
    cd /opt/opencrowbar/core/rails
     tasks=("rake db:drop" "rake db:create" "rake railties:install:migrations"
            "rake db:migrate" "rake db:seed")
    for task in "${tasks[@]}"; do
        bundle exec $task
    done
    bundle exec rake test && bundle exec rspec
)
. /etc/profile
# Do BDD tests first
yum -y install erlang
test_bdd && test_rake
