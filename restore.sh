#!/usr/bin/env bash

# Example restore script for the data that Rebar needs to be able to run.
# Restore should be done with all DigitalRebar containers stopped.
# Restore sequence should be as follows:
# 1: Restore contents of $HOME/.cache/digitalrebar/tftpboot
# 2: Restore local deploy config information
# 3: Restore the local Consul database information, start Consul container.
# 4: Start the database container, wait for postgres to be active, and restore databases.
# 5: Restore the Goiardi container file data, start Goiardi container
# 6: Restart the rest of the containers.
#
# This script handles all of the items in that order, optionally
# skipping the tftpboot stuff if there is no tftpboot backup.


# Any backup processes should be run while DigitalRebar is up and
# running.

if [[ ! -f compose/docker-compose-common.yml ]]; then
    echo "$0 must run from the top-level deploy directory"
    exit 1
fi


if [[ ! -f $1/sha512sums ]]; then
    echo "$1 does not contain a sha512sums file!"
    exit 1
fi

(cd "$1"; sha512sum -c sha512sums) || exit 1

required_files=(deploy_files.tar.gz consul.tar.gz pg_dump.gz
                pg_hba.conf pg_ident.conf postgresql.auto.conf
                postgresql.conf postmaster.opts
                goiardi_files.tar.gz)

for f in "${required_files[@]}"; do
    [[ -f $1/$f ]] && continue
    echo "Missing required backup file $f"
    failed=true
done

[[ $failed ]] && exit 1

in_docker() {
    # $1 = container name as understood by docker-compose
    # $rest = command to run in the container
    local container="compose_$1_1"
    shift
    docker exec "$container" "$@"
}

# Wipe out any currently-running containers
(cd compose; docker-compose kill; docker-compose rm -f) || :

# Clean up crap we will restore over the top of.
clean_files=(access.env config-dir data-dir docker-compose.yml services.env)
(cd compose; sudo rm -rf "${clean_files[@]}")

# Extract the contents of the tftpboot directory.  This is the only
# one we don't complain about if it does not exist
if [[ -f $1/tftpboot.tar.gz ]]; then
    echo "Restoring tftpboot"
    mkdir -p "$HOME/.cache/digitalrebar"
    zcat "$1/tftpboot.tar.gz" | tar -C "$HOME/.cache/digitalrebar" -x -f -
fi

# Restore the local configuration options
echo "Restoring docker-compose config info"
zcat "$1/deploy_files.tar.gz" |tar -C compose -x -f -

# Restore the Consul database, then start the Consul database
sudo mkdir -p compose/data-dir
echo "Restoring Consul data"
zcat "$1/consul.tar.gz" |sudo tar -C compose/data-dir -x -f -

# Arrange for the postgres container to restore itself from backup
echo "Restoring Postgres data"
sudo mkdir -p compose/data-dir/postgresql/backup
for f in pg_dump.gz pg_hba.conf pg_ident.conf postgresql.auto.conf postgresql.conf postmaster.opts; do
    sudo cp "$1/$f" "compose/data-dir/postgresql/backup/$f"
done

# Restore the Goiardi file data, then start Goiardi
echo "Restoring Goiardi data"
zcat "$1/goiardi_files.tar.gz" |sudo tar -C compose/data-dir -x -f -

# Start everything else
echo "Restarting Digital Rebar"
(cd compose; docker-compose up -d)
