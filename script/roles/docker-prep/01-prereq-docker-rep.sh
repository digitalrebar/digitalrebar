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
# install python API
curl https://bootstrap.pypa.io/get-pip.py -o /tmp/get-pip.py
python /tmp/get-pip.py
pip install docker-py

# setup proxy
mkdir /etc/systemd/system/docker.service.d
echo "[Service]" > /etc/systemd/system/docker.service.d/http-proxy.conf
echo "Environment=\"HTTP_PROXY=${http_proxy}\"" >> /etc/systemd/system/docker.service.d/http-proxy.conf
sudo systemctl daemon-reload

# bounce to be safe
sudo systemctl restart docker
# make Docker work without reboot
sudo chmod 666 /var/run/docker.sock
