#!/bin/bash
if [[ -f /etc/os-release ]]; then
  . /etc/os-release
fi

if ! which docker; then
    # install docker using download script
    curl -sSL https://get.docker.com/ -o /tmp/docker.sh
    chmod +x /tmp/docker.sh
    /tmp/docker.sh
else
    echo "docker already installed, skipping"
fi

# install python API
if ! which pip; then
    curl https://bootstrap.pypa.io/get-pip.py -o /tmp/get-pip.py
    python /tmp/get-pip.py
fi

if ! pip show docker-py; then
    pip install docker-py
else
    echo "docker python API already installed, skipping"
fi

# setup proxy
if [[ -f /etc/redhat-release || -f /etc/centos-release ]]; then
    if [[ -f /etc/systemd ]]; then
        if [[ ! -f /etc/systemd/system/docker.service.d/http-proxy.conf ]]; then
            mkdir -p /etc/systemd/system/docker.service.d
            echo "[Service]" > /etc/systemd/system/docker.service.d/http-proxy.conf
            echo "Environment=\"HTTP_PROXY=${http_proxy}\"" >> /etc/systemd/system/docker.service.d/http-proxy.conf
            echo "Environment=\"HTTPS_PROXY=${https_proxy}\"" >> /etc/systemd/system/docker.service.d/http-proxy.conf
            systemctl daemon-reload
        else
            echo "docker proxy already set, skipping"	
        fi
    else
	echo "http_proxy=\"${http_proxy}\"" >> /etc/sysconfig/docker
	echo "export http_proxy" >> /etc/sysconfig/docker
	echo "https_proxy=\"${https_proxy}\"" >> /etc/sysconfig/docker
	echo "export https_proxy" >> /etc/sysconfig/docker
    fi
elif [[ -d /etc/apt ]]; then
    echo "export http_proxy=\"${http_proxy}\"" >> /etc/default/docker
    echo "export https_proxy=\"${https_proxy}\"" >> /etc/default/docker
fi

# bounce to be safe
service docker restart

# make Docker work without reboot
sudo chmod 666 /var/run/docker.sock
