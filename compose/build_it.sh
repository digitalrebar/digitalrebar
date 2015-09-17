#!/bin/bash

declare -a containers=('postgres'
            'webproxy'
            'goiardi'
            'rebar_api'
            'ntp'
            'dns'
            'dhcp'
            'provisioner'
            'node')

# Clean tagged containers
echo "Removing tagged containers"
for i in "${containers[@]}"
do
  docker rmi "galthaus/dr_$i"
done

# Clean up the containers
docker ps -q -a | xargs docker rm

# Clean compose containers
if [ "$1" == "--clean" ] ; then
  echo "Removing compose containers"
  docker rmi $(docker images | grep compose | awk '{ print $3 }')
fi

# Build containers
docker-compose -f docker-compose-build.yml build

# Make small versions
#for i in "${containers[@]}"
#do
#  echo "Compressing (by import/export) $i"
#  docker create --name test_$i compose_$i
#  docker export test_$i | docker import - compose_${i}_small
#  docker rm test_$i
#done

# Tag containers
for i in "${containers[@]}"
do
  echo "Tagging $i"
  docker tag compose_${i} galthaus/dr_$i
done

# Push containers
for i in "${containers[@]}"
do
  echo "Pushing $i"
  docker push galthaus/dr_$i
done

