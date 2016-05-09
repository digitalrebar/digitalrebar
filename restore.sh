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

if ! [[ $1 ]]; then
    echo "Must pass the directory to restore to as the first argument to $0"
    exit 1
fi

if [[ -e $1 || -d $1 ]]; then
    echo "$1 must not already exist"
    exit 1
fi


missing=()
for cmd in gzip sha512sum xz uuidgen docker docker-compose; do
    which "$cmd" &>/dev/null || missing+=("$cmd")
done

if [[ $missing ]]; then
    echo "Missing required commands: ${missing[*]}"
    exit 1
fi


if [[ ! -f sha512sums ]]; then
    echo "$PWD does not contain a sha512sums file!"
    exit 1
fi

sha512sum -c sha512sums || exit 1

required_files=(deploy_files.tar.xz consul.tar.xz pg_dump.gz
                pg_hba.conf pg_ident.conf postgresql.auto.conf
                postgresql.conf postmaster.opts
                goiardi_files.tar.xz restore.sh)

for f in "${required_files[@]}"; do
    [[ -f $f ]] && continue
    echo "Missing required backup file $f"
    failed=true
done

[[ $failed ]] && exit 1

dest=$1

mkdir -p "$dest"

in_docker() {
    # $1 = container name as understood by docker-compose
    # $rest = command to run in the container
    local container="compose_$1_1"
    shift
    docker exec "$container" "$@"
}

# restore source code
xz -d -c source.tar.xz | tar -C "$dest" -x -f -

if [[ ! -d $dest/deploy/compose ]]; then
    echo "Backup missing compose information, cannot continue"
    exit 1
fi


# Restore local images
if [[ -f images.tar.xz ]]; then
    echo "Restoring docker images (this may take awhile)"
    xz -d -c images.tar.xz |docker load
fi
# Wipe out any currently-running containers
(cd "$dest/deploy/compose"; docker-compose kill; docker-compose rm -f) || :

# Clean up crap we will restore over the top of.
clean_files=(access.env config-dir data-dir docker-compose.yml services.env)
(cd "$dest/deploy/compose"; sudo rm -rf "${clean_files[@]}")

# Extract the contents of the tftpboot directory.  This is the only
# one we don't complain about if it does not exist
if [[ -f $1/tftpboot.tar.xz ]]; then
    echo "Restoring tftpboot"
    mkdir -p "$HOME/.cache/digitalrebar"
    xz -d -c "$1/tftpboot.tar.xz" | tar -C "$HOME/.cache/digitalrebar" -x -f -
fi
# Restore the local configuration options
echo "Restoring docker-compose config info"
xz -d -c deploy_files.tar.xz |tar -C "$dest/deploy/compose" -x -f -

if [[ -f tag ]]; then
    export DR_TAG="$(cat tag)"
    cp tag "$dest/deploy/compose/tag"
fi

(cd "$dest/deploy/compose"; docker-compose create postgres consul goiardi)
# Restore the Consul database, then start the Consul database
sudo mkdir -p "$dest/deploy/compose/data-dir"
echo "Restoring Consul data"
xz -d -c consul.tar.xz |sudo tar -C "$dest/deploy/compose/data-dir" -x -f -
(cd "$dest/deploy/compose"; docker-compose start consul)

# Arrange for the postgres container to restore itself from backup
echo "Restoring Postgres data"
sudo mkdir -p "$dest/deploy/compose/data-dir/postgresql/backup"
for f in pg_dump.gz pg_hba.conf pg_ident.conf postgresql.auto.conf postgresql.conf postmaster.opts; do
    sudo cp "$f" "$dest/deploy/compose/data-dir/postgresql/backup/$f"
done
(cd "$dest/deploy/compose"; docker-compose start postgres)
printf "Waiting on database restore to finish"
while [[ -d "$dest/deploy/compose/data-dir/postgresql/backup" ]]; do
    sleep 5
    printf '.'
done
echo " Done."

# Restore the Goiardi file data, then start Goiardi
echo "Restoring Goiardi data"
xz -d -c goiardi_files.tar.xz |sudo tar -C "$dest/deploy/compose/data-dir" -x -f -

# Start everything else.
echo "Restarting Digital Rebar"
cd "$dest/deploy/compose"
# If we need the forwarder, it will already be created.
# If we need it, we have to start it first so that the dhcp container
# (which uses the forwarder's network namespace) can properly link things together.
if docker-compose config --services |grep -q forwarder; then
    docker-compose start forwarder
fi
# Avoid recreating containers we just crated and started.
docker-compose create --no-recreate
docker-compose start
