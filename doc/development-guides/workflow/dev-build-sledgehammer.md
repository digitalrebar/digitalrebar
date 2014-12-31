Sledgehammer is the component of OpenCrowbar that we use as a bootstrapping 
tool to perform initial node discovery and to register its discovered
state within the OpenCrowbar node provisioning framework. After it has been
discovered Sledehammer can be controlled to prepare the hardward and to 
lay down the operating system and to assure correct node configuration.
It consists of a slightly modified Centos 6.5 live environment.  To build
Sledgehammer you need:

  * A CentOS 6.5 install DVD from bittorrent or your favorite CentOS
    mirror. 

To create Sledgehammer, run the `tools\build_sledgehammersh` script in the main
OpenCrowbar core checkout.

### Failures

If it fails to find some packages, change the mirror kickstart is using in `sledgehammer\sledgehammer.ks`

List of mirrors here: http://isoredirect.centos.org/centos/6/isos/x86_64/

#### Example diff

```diff
diff --git a/sledgehammer/sledgehammer.ks b/sledgehammer/sledgehammer.ks
index 27fc7c0..7f8aa2f 100644
--- a/sledgehammer/sledgehammer.ks
+++ b/sledgehammer/sledgehammer.ks
@@ -7,9 +7,9 @@ rootpw --iscrypted $1$H6F/NLec$Fps2Ut0zY4MjJtsa1O2yk0
 selinux --disabled
 firewall --disabled
 
-repo --name=a-base    --baseurl=http://mirror.centos.org/centos/6/os/$basearch
-repo --name=a-updates --baseurl=http://mirror.centos.org/centos/6/updates/$basearch
-repo --name=a-extras  --baseurl=http://mirror.centos.org/centos/6/extras/$basearch
+repo --name=a-base    --baseurl=http://mirrors.kernel.org/centos/6/os/$basearch
+repo --name=a-updates --baseurl=http://mirrors.kernel.org/centos/6/updates/$basearch
+repo --name=a-extras  --baseurl=http://mirrors.kernel.org/centos/6/extras/$basearch
 repo --name=a-live    --baseurl=http://www.nanotechnologies.qc.ca/propos/linux/centos-live/$basearch/live
 repo --name=a-wsman   --baseurl=http://download.opensuse.org/repositories/Openwsman/CentOS_CentOS-6
 %packages
```
