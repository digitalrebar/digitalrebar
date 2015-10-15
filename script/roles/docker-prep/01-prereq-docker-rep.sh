#!/bin/bash

# docker installed?
if ! which docker &>/dev/null; then
	# install docker using download script
	curl -sSL https://get.docker.com/ -o /tmp/docker.sh
	sudo chmod +7 /tmp/docker.sh
	sudo /tmp/docker.sh
else
	echo "docker already installed, skipping"
fi
# bounce to be safe
sudo service docker restart
# make Docker work without reboot
sudo chmod 666 /var/run/docker.sock