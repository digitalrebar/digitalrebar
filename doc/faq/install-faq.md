Q: `crowbar converge` keeps failing with httpd installs when running `./tools/docker-admin centos ./production.sh my.domain.here`

Converge failed, so I checked the logs and saw Chef failing to install httpd.  So I ran it on the command line and got the full error:
```
error: unpacking of archive failed on file /usr/sbin/suexec: cpio: cap_set_file

newgoliath [3:14 PM] Installing : httpd-2.2.22-1.ceph.el6.x86_64                                                                   1/1
Error unpacking rpm package httpd-2.2.22-1.ceph.el6.x86_64
error: unpacking of archive failed on file /usr/sbin/suexec: cpio: cap_set_file

newgoliath [3:14 PM] Linux judd-m6600 3.13.0-32-generic #57-Ubuntu SMP Tue Jul 15 03:51:08 UTC 2014 x86_64 x86_64 x86_64 GNU/Linux

newgoliath [3:16 PM]3:16 bash-4.1# sestatus
SELinux status:                 disabled 
```


A: Turns out it has nothing to do with SELinux.  Make sure your docker server is running with `-s devicemapper`.  On Ubuntu edit `/etc/default/docker` to include it.  On RedHat derived, edit `/etc/sysconfig/docker`.  All better!

---

Q: during `crowbar converge` dhcpd refuses to start.  In `/var/log/messages` you find gems like:
```
nodes/c5babcfd3233.log:Aug 25 23:09:42 c5babcfd3233 kernel: [29976.767673] type=1400 audit(1409004582.401:82): apparmor="DENIED" operation="open" profile="/usr/sbin/dhcpd" name="/etc/dhcp3/groups.d/group_list.conf" pid=32074 comm="dhcpd" requested_mask="r" denied_mask="r" fsuid=0 ouid=0
nodes/c5babcfd3233.log:Aug 25 23:35:18 c5babcfd3233 dhcpd: Internet Systems Consortium DHCP Server 4.1.1-P1
nodes/c5babcfd3233.log:Aug 25 23:35:18 c5babcfd3233 dhcpd: Copyright 2004-2010 Internet Systems Consortium.
nodes/c5babcfd3233.log:Aug 25 23:35:18 c5babcfd3233 dhcpd: All rights reserved.
nodes/c5babcfd3233.log:Aug 25 23:35:18 c5babcfd3233 dhcpd: For info, please visit https://www.isc.org/software/dhcp/
nodes/c5babcfd3233.log:Aug 25 23:35:18 c5babcfd3233 dhcpd: Can't open /etc/dhcp3/groups.d/group_list.conf: Permission denied
```


A: That's because your underlying Ubuntu OS has apparmor protecting the dhcpd package you have installed on the underlying Ubuntu.  For Crowbar to work in Docker, you have to remove that apparmor stuff.  If you still need the DHCPd server, just try removing the apparmor related files (ymmv).  I revmove the whole package, which I found by doing

On the host: `dpkg -S /etc/apparmor.d/usr.sbin.dhcpd` and then `dpkg --purge isc-dhcp-server`.  I then needed to `service apparmor restart`

In the container: `service dhcpd restart`  and it turned out [OK]
