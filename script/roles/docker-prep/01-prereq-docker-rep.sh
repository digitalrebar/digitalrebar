#!/bin/bash
if [[ -f /etc/os-release ]]; then
  . /etc/os-release
fi

if ! which docker; then
    if [[ -f /etc/redhat-release || -f /etc/centos-release ]]; then
		# install docker using download script
		curl -sSL https://get.docker.com/ -o /tmp/docker.sh
		chmod +x /tmp/docker.sh
		/tmp/docker.sh
    elif [[ -d /etc/apt ]]; then
        # Make need extra repos
        apt-get install -y linux-image-extra-`uname -r`
        apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
        sh -c "echo deb https://apt.dockerproject.org/repo ubuntu-trusty main > /etc/apt/sources.list.d/docker.list"
        apt-get update
        apt-get install -y docker-engine
    elif [[ -f /etc/SuSE-release ]]; then
        zypper install -y -l docker
    elif [[ "x$NAME" == "xCoreOS" ]]; then
        echo "Already installed"
    else
        die "Staged on to unknown OS media!"
    fi
else
	echo "docker already installed, skipping"
fi

# install python API
if ! which pip; then
	curl https://bootstrap.pypa.io/get-pip.py -o /tmp/get-pip.py
	python /tmp/get-pip.py
	pip install docker-py
else
	echo "docker python API already installed, skipping"
fi

# setup proxy
if [[ -f /etc/redhat-release || -f /etc/centos-release ]]; then
	if [[ ! -f /etc/systemd/system/docker.service.d/http-proxy.conf ]]; then
		mkdir -p /etc/systemd/system/docker.service.d
		echo "[Service]" > /etc/systemd/system/docker.service.d/http-proxy.conf
		echo "Environment=\"HTTP_PROXY=${http_proxy}\"" >> /etc/systemd/system/docker.service.d/http-proxy.conf
		if [[ -f /etc/redhat-release || -f /etc/centos-release ]]; then
			systemctl daemon-reload
		fi
	else
		echo "docker proxy already set, skipping"	
	fi
elif [[ -d /etc/apt ]]; then
	echo "export http_proxy=\"${http_proxy}\"" >> /etc/default/docker
fi

# bounce to be safe
if [[ -f /etc/redhat-release || -f /etc/centos-release ]]; then
	systemctl restart docker
elif [[ -d /etc/apt ]]; then
	service docker restart
fi

# make Docker work without reboot
sudo chmod 666 /var/run/docker.sock
