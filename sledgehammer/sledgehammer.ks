lang en_US.UTF-8
keyboard us
timezone US/Eastern
auth --useshadow --enablemd5
# crowbar
rootpw --iscrypted $1$H6F/NLec$Fps2Ut0zY4MjJtsa1O2yk0
selinux --disabled
firewall --disabled

repo --name=a-base    --baseurl=http://mirrors.kernel.org/centos/6/os/$basearch
repo --name=a-updates --baseurl=http://mirrors.kernel.org/centos/6/updates/$basearch
repo --name=a-extras  --baseurl=http://mirrors.kernel.org/centos/6/extras/$basearch
repo --name=a-live    --baseurl=http://www.nanotechnologies.qc.ca/propos/linux/centos-live/$basearch/live
repo --name=a-wsman   --baseurl=http://download.opensuse.org/repositories/Openwsman/CentOS_CentOS-6
repo --name=a-lldpd   --baseurl=http://download.opensuse.org/repositories/home:/vbernat/CentOS_CentOS-6/
%packages
@core
OpenIPMI
OpenIPMI-tools
authconfig
autoconf
automake
bash
chkconfig
compat-libstdc++-33.i686
comps-extras
coreutils
curl
dhclient
dmidecode
efibootmgr
gcc
gcc-c++
git
gzip
kernel
libstdc++.i686
libsysfs.x86_64
libxml2
libxml2-devel
libxml2.i686
libxslt
lldpd
lvm2
make
mdadm
mktemp
ntp
openssh-clients
openssh-server
openwsman-ruby
parted
passwd
pciutils
perl-XML-Twig
policycoreutils
rootfiles
rpm
ruby
ruby-devel.x86_64
ruby-libs.x86_64
ruby-rdoc
ruby-ri
rubygems
screen
syslinux
tar
tcpdump
unzip
vconfig
vim-enhanced
wget
which
wsmancli
yum
zlib
zlib-devel

%post

# Hack to really turn down SELINUX
sed -i -e 's/\(^SELINUX=\).*$/\1disabled/' /etc/selinux/config

########################################################################
# Create a sub-script so the output can be captured
# Must change "$" to "\$" and "`" to "\`" to avoid shell quoting
########################################################################
cat > /root/post-install << EOF_post
#!/bin/bash

echo ###################################################################
echo ## Creating the centos-live init script
echo ###################################################################

cat > /etc/rc.d/init.d/openstack-start-up << EOF_initscript
#!/bin/bash
#
# live: Init script for live image
#
# chkconfig: 345 72 28
# description: Init script for live image.
screen -d -m -S sledgehammer-bootstrap -t 'Sledgehammer Bootstrap' \
    script -f -c '/sbin/sledgehammer-start-up.sh' /var/log/install.log


EOF_initscript

/sbin/chkconfig --add openstack-start-up

EOF_post

/bin/bash -x /root/post-install 2>&1 | tee /root/post-install.log


%post --nochroot

########################################################################
# Create a sub-script so the output can be captured
# Must change "$" to "\$" and "`" to "\`" to avoid shell quoting
########################################################################
cat > /root/postnochroot-install << EOF_postnochroot
#!/bin/bash

cp start-up.sh $INSTALL_ROOT/sbin/sledgehammer-start-up.sh
chmod +x $INSTALL_ROOT/etc/rc.d/init.d/openstack-start-up
chmod +x $INSTALL_ROOT/sbin/sledgehammer-start-up.sh
cp sshd_config $INSTALL_ROOT/etc/ssh/sshd_config

cp dhclient.conf $INSTALL_ROOT/etc

EOF_postnochroot

/bin/bash -x /root/postnochroot-install 2>&1 | tee /root/postnochroot-install.log

%end
