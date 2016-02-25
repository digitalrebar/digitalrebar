#!/usr/bin/env bash

# Example backup script for the data that Rebar needs to be able to run.
# This boils down to 3 sets of data:
# 1: The data saved in Consul
# 2: The data in our local Postgres database
# 3: The file data for Goiardi
# 4: The local deploy config information
# 5: The contents of $HOME/.cache/digitalrebar/tftpboot
#
# This script handles getting the first 5 items, getting the third is
# left as an exercise for the reader.
#
# Any backup processes should be run while DigitalRebar is up and
# running.

if [[ ! -d compose/config-dir ]]; then
    echo "$0 must run from the top-level deploy directory"
fi

if [[ ! $1 ]]; then
    echo "First arg to this script must be a directory that the backup should be saved to!"
    echo "The directory must not exist beforehand"
    exit 1
fi

if [[ -e $1 ]]; then
    echo "$1 already exists, refusing to do anything"
    exit 1
fi

# Some useful utility functions

in_docker() {
    # $1 = container name as understood by docker-compose
    # $rest = command to run in the container
    local container="compose_$1_1"
    shift
    docker exec "$container" "$@"
}

mkdir -p "$1"

# Back up local Consul database
echo "Backing up Consul"
in_docker consul pkill -STOP consul
sudo tar -C compose/data-dir -c -f - consul | gzip -9 >"$1/consul.tar.gz"
in_docker consul pkill -CONT consul

# Back up Postgres database
echo "Backing up Postgres"
for f in pg_hba.conf pg_ident.conf postgresql.auto.conf postgresql.conf postmaster.opts; do
    in_docker postgres cat "/var/lib/postgresql/data/$f" >"$1/$f"
done
in_docker postgres su -c 'pg_dumpall -c' postgres |gzip -9 >"$1/pg_dump.gz"

# Back up Goiardi file data
echo "Backing up Goiardi"
in_docker goiardi pkill -STOP goiardi
sudo tar -C compose/data-dir -c -f - goiardi |gzip -9 >"$1/goiardi_files.tar.gz"
in_docker goiardi pkill -CONT goiardi

# Back up the local config information
echo "Saving docker-compose config information"
tar -C compose -c docker-compose.yml access.env services.env config-dir |gzip -9 >"$1/deploy_files.tar.gz"

# Backup the tftpboot directory
# echo "Backing up tftpboot"
# sudo tar -C "$HOME/.cache/digitalrebar" -c -f - tftpboot |gzip -9 > "$1/tftpboot.tar.gz"

# Create checksums to verify file integrity for restore purposes
(cd "$1"; sha512sum * >sha512sums)
