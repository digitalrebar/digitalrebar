lang en_US.UTF-8
keyboard us
timezone UTC
auth --useshadow --enablemd5
# rebar1
rootpw --iscrypted $1$UwJdGUMy$ORqjDQIW//wt7sWY.xG9M0
selinux --disabled
firewall --disabled

repo --name=a-base    --baseurl=http://mirrors.kernel.org/centos/7/os/$basearch
repo --name=a-updates --baseurl=http://mirrors.kernel.org/centos/7/updates/$basearch
repo --name=a-extras  --baseurl=http://mirrors.kernel.org/centos/7/extras/$basearch
repo --name=a-lldpd   --baseurl=http://download.opensuse.org/repositories/home:/vbernat/CentOS_7/
repo --name=a-epel    --baseurl=http://mirrors.kernel.org/fedora-epel/7/$basearch

%packages
OpenIPMI
OpenIPMI-tools
aic94xx-firmware
audit
authconfig
basesystem
bash
bzr
comps-extras
coreutils
curl
dhclient
dmidecode
dpkg
e2fsprogs
efibootmgr
file
filesystem
firewalld
glibc
glibc.i686
gzip
hostname
initscripts
iproute
iprutils
iptables
iputils
jq
kbd
kernel
kernel-tools
kexec-tools
less
libsysfs
linux-firmware
lldpd
lshw
lvm2
man-db
mdadm
microcode_ctl
mktemp
ncurses
ntp
openssh-clients
openssh-server
openssl-libs
parted
passwd
pciutils
plymouth
policycoreutils
procps-ng
python-pip
rdma
rootfiles
rpm
rsyslog
ruby
ruby-devel.x86_64
ruby-libs.x86_64
rubygems
setup
shadow-utils
stress
stress-ng
sudo
syslinux
systemd
tar
tcpdump
unzip
util-linux
vconfig
vim-enhanced
vim-minimal
wget
which
yum
zlib
%end

%post --nochroot

########################################################################
# Create a sub-script so the output can be captured
# Must change "$" to "\$" and "`" to "\`" to avoid shell quoting
########################################################################
cat > /root/postnochroot-install << EOF_postnochroot
#!/bin/bash

cp start-up.sh $INSTALL_ROOT/sbin/sledgehammer-start-up.sh
chmod +x $INSTALL_ROOT/sbin/sledgehammer-start-up.sh
cp sshd_config $INSTALL_ROOT/etc/ssh/sshd_config
cp curtin.tgz $INSTALL_ROOT/root/curtin.tgz

cp dhclient.conf $INSTALL_ROOT/etc

EOF_postnochroot

/bin/bash -x /root/postnochroot-install 2>&1 | tee /root/postnochroot-install.log
%end

%post

# Hack to really turn down SELINUX
sed -i -e 's/\(^SELINUX=\).*$/\1disabled/' /etc/selinux/config
systemctl enable network
systemctl disable kdump

########################################################################
# Create a sub-script so the output can be captured
# Must change "$" to "\$" and "`" to "\`" to avoid shell quoting
########################################################################
cat > /root/post-install << EOF_post
#!/bin/bash

echo ###################################################################
echo ## Creating the centos-live init script
echo ###################################################################

cat > /etc/systemd/system/sledgehammer.service << EOF_initscript
[Unit]
Description=Sledgehammer startup script
After=network-online.target
Wants=network-online.target

[Service]
Type=oneshot
RemainAfterExit=true
ExecStart=/sbin/sledgehammer-start-up.sh

[Install]
WantedBy=multi-user.target

EOF_initscript

systemctl enable sledgehammer.service

EOF_post

/bin/bash -x /root/post-install 2>&1 | tee /root/post-install.log

echo "nameserver 8.8.8.8" > /etc/resolv.conf
curl -fgLO https://opscode-omnibus-packages.s3.amazonaws.com/el/6/i686/chef-11.18.12-1.el6.i686.rpm
yum install -y chef-11.18.12-1.el6.i686.rpm
rm chef-11.18.12-1.el6.i686.rpm

# Setup and install curtin
tar -xvf /root/curtin.tgz
cd trunk.dist
pip install -r requirements.txt
python ./setup.py install
cd ..
rm -rf trunk.dist curtin.tgz

rm -f /etc/resolv.conf

%end
