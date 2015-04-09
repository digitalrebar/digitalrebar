# RHEL/CentOS 6.6 Deployment Guide

## Objectives

Create an OpenCrowbar admin node on a VM or physical machine to begin the process of configuring or managing deployment of a cluster (openstack, hadoop, ceph, etc.).

*NOTE*: OpenCrowbar assumes complete ownership of the admin node and is
*NOT* meant to be a shared services machine.  The installation process
will turn off firewalls and turn off selinux.  The installation process
will reduce exposed ports and restrict ssh access.

The following steps will be completed:

    * Prepare a Virtual or Physical machine
    * Installation of CentOS 6.6 x86_64
    * Install OpenCrowbar
    * Start OpenCrowbar webUI
    * Connect to the webUI using a browser

An outline is provided as a foundation for QA validation requirements for OpenCrowbar RPM packages.

Known limitations of the installation process, its sensitivities to updates and to upgrades is summarized.

## Prerequisites

Before commencing installation and configuration, ensure that everything needed is available and that all remote resources that must be accessed are capable of being reached.

   * CentOS 6.6 x86_64 - download site: http://www.centos.org/download/
   * You will need to know how to access the internet from your VM/Physical environment.
   * Optional: Proxy Services
       * Windows - Fiddler 2 is a good one
       * Linux - potential proxy services include: cntlm and squid.
       * Direct Connection - Ensure you have appropriate security setup per security guidelines in effect within your organization.

## Machine preparation

Machine requirements are:

   * Memory: Min 4GB
   * CPU Cores: 2 or more
   * Network Interface Controllers: 2 preferred, 1 minimum (can use virtio if using a VM)
      * The first NIC (may be named eth0, em1, or en1) must be wired into the private space (192.168.124.0/24)
      * The second NIC will be wired into a network that routes to the internet. Internet access is required for installation of CentOS/RHEL 6.6
      * Note: It is possible to use a single NIC. In that case the default network address will be 192.168.124.0/24, the admin node IP address will be 192.168.124.10
      * Where a single NIC is used, the private admin network (192.168.124.0/24) must be capable of download of files from the internet or from a local caching server
   * Storage: A disk capacity of at least 80 GB is preferred. 
   * Make sure you configure RAID on the drives before installing.
 
Ensure that all physical network transports are correctly configured and are operational - at least check/verify that:
   * All network cabling is in place
   * Network switches are turned on and configured correctly (ask network admin if necessary)
   * Keyboard,Video, and Mouse (if required) devices are connected and ready for use.
 
If using a virtual machine (VM), where VM motion (ability to migrated VMs across Hypervisor platforms) is required ensure that secure VM access is correctly configured and is operational.

Where network-managed power switches are in use, ensure that network access is secure from unwanted access.

## CentOS 6.6 installation

The following is a screen/selector step process to get CentOS 6.6
installed:

   * Boot CentOS 6.6 x86_64 from pristine ISO media
   * At the boot screen select "Install or upgrade an existing system", hit Enter
   * Screen: "Welcome to CentOS for x86_64", select [Skip], hit Enter
   * At the first graphical screen, "CentOS 6 Community ENTerprise Operating System", Click [Next]
   * Screen: "What language would you like to use ...", Select "English (English)", Click [Next]
   * Screen: "Select the appropriate keyboard ...", Select "U.S. English", Click [Next]
   * Screen: "What type of devices will your installation involve\?", Select "Basic Storage Devices", Click [Next]
   * Pop-up: "Storage Device Warning", Click [Yes, discard any data]
   * Screen: "Please name this computer. The hostname ...", In the Hostname field enter as valid FQDN (Ex. admin.mytest.lcl)
   * Click [Configure Network]
      * Network configuration requirements depend on how many NICs are available, and on available network topology
      * For Single NIC configuration:
         * Select "System eth0" (first NIC - on public network), Click [Edit]
            * Check "Connect automatically"
            * Click on [IPV4 Settings]
            * Select Method "Manual"
               * Click [Add]
                  * Enter IP address: 192.168.124.10
                  * Click on blank field below "Netmask". Enter: 24, or 255.255.255.0.
                  * Enter DNS ip address (Ex.38.151.210.40)
            * Click [Apply]
      * For Dual NIC configuration:
        * Select "System eth0" (First NIC - on private admin network), Click [Edit]
            * Check "Connect automatically"
            * Click [IPV4 Settings]
            * Select Method "Manual"
               * Click [Add]
                  * Enter IP address: 192.168.124.10
                  * Click on blank field below "Netmask". Enter: 24, or 255.255.255.0, no gateway
                  * Enter DNS ip address (Ex.38.151.210.40)
            * Click [Apply] 
        * Select "System eth1" (Second NIC - on public network), Click [Edit]
            * Check "Connect automatically"
            * Click [IPV4 Settings]
            * Select Method "Automatic (DHCP)" if appropriate, else configure network settings. (Need ip, netmask and gw)
            * Click [Apply]
          * Click [Close]
      * Click [Next]
   * Screen: "Please select the nearest city in your time zone:", Select your time zone, Click [Next]
   * Screen: "The root account is used for administering the system. ...", Enter  a Root password (Ex. crowbar), Confirm , Click [Next]
   * Popup: "Weak Password", Click [Use Anyway]
   * Screen: "Which type of installation would you like?", Select "Use All Space", Click [Next]
   * Popup: "Writing storage configuration to disk", Click [Write changes to disk]
   * Screen: "The default installation of CentOS is a minimum install ...", Select "Basic Server", Click [Next]
   * The system will now install. When finished, Click [Reboot]

## OpenCrowbar installation

To install OpenCrowbar, the following things need to be done:
   * Turn off firewalls
   * Turn off or set SELinux to permissive
   * Download the default installation OS [Optional]
 
Remote Access Step (Do one of the following):
   * Install an admin user (e.g. adduser admin) and make sure sudo works
   * Install an ssh public key in the root directory /root/.ssh/authorized_keys file
     to enable key-less root access
   * Use the crowbar user that is installed by the production.sh script to access the box after installation.  The default password is crowbar.

The standard install will remove access to root account using password.  Key-less access is allowed.

The crowbar-install.sh script supports three flags given (or not in this
order):
   * Select release (if not specificed, it is master).  The following options:
      * --develop - Use RPMs built from the develop tree.
      * --master - Use RPMs built from the master tree. This is same as not specified.
      * --release <name> - Use RPMs built from the release <name> tree.
   * --without-hardware - Don't install the hardware RPM
   * --download-os - This will download the Centos-7.1.1503 ISO for
installation of nodes. By default, this is not done.

After logging in as root, run the following command (as an example) to
install hardware support off of the latest master build:

```
wget --no-check-certificate -O -
https://raw.githubusercontent.com/opencrowbar/core/develop/tools/crowbar-install.sh | source /dev/stdin
```

or if you don't want hardware support (bios, RAID, and IPMI) and the
develop builds, run the following command:

```
wget --no-check-certificate -O -
https://raw.githubusercontent.com/opencrowbar/core/develop/tools/crowbar-install.sh
| source /dev/stdin --develop --without-hardware
```

This will take a little bit of time.  Once complete, you will need to
add the RAID configuration tools displayed as output or described 
[here](https://github.com/opencrowbar/hardware/tree/master/doc).

You may also want to add supported ISOs to your installation as
described
[here](https://github.com/opencrowbar/core/tree/master/doc/deployment-guide/adding-operating-systems.md).

If you are running on a VM, you may want to snapshot the VM support
updates, see below.

## OpenCrowbar Configuration

### Services

By default OpenCrowbar will configure DNS, NTP, and DHCP to run on the admin node.  You can choose to change the configuration of these services by modifying /opt/opencrowbar/core/crowbar-config.sh.  These include setting up forwarding DNS servers and upstream stratum servers for NTP.

It is also possable to utilize services that are already installed in the enviornment and choose to have OpenCrowbar not install them on the Admin server.  These configuirations are described [here] (https://github.com/opencrowbar/core/tree/master/doc/deployment-guide/external-services.md)

Execute the following commands:

```
  cd /opt/opencrowbar/core
  ./production.sh <FQDN of the admin node>
```

Once this is complete, the admin node is configured.  

## Start Using OpenCrowbar

Launch your web browser and connect to the IP address of the Admin node on port 3000 using a browser of choice (Google Chrome, or Internet Explorer) URL:http://192.168.124.10:3000

   * Log in as user: crowbar
   * Password: crowbar

## Known Issues:

### Updating OpenCrowbar

There is not an update method currently.  We don't handle database
migration or chef updates currently.  Please use the following to
replace the system.

  1. Revert your VM to the last Snapshot taken,  (You did follow the advice above to make a SnapShot, correct?)
  1. yum clean all; yum makecache
  1. yum erase opencrowbar-hardware # If it was installed
  1. yum erase opencrowbar-core
  1. yum install -y opencrowbar-core
  1. yum install -y opencrowbar-hardware # If you want hardware support
  1. cd /opt/opencrowbar/core
  1. ./production.sh <FQDN>
  1. Launch your web browser and connect to the IP address of the Admin node on port 3000 using a browser of choice (Google Chrome, or Internet Explorer) URL:http://192.168.124.10:3000

## Known Limitations:

Please document all limitations that are discovered into this document.

RPM package installation/removal/update/upgrade processes confer many known limitations on third-party application-layer services such as OpenCrowbar, OpenStack, Hadoop. Here are a few issues that need to be defined and addressed:

  * There is a latent need to document update and upgrade requirements and dependencies so that packaging methods can fully accommodate the scope of these so far as possible.
  * The impact of RPM package updates on service continuity must be clearly defined. User-oriented documentation should set appropriate expectations for RPM update application.
  * Risks to continuity of service, potential for loss of critical operational data needs to be identifies and documented.

Testing, validation and QA requirements for OpenCrowbar itself need to be documented separately and links to these documents should be inserted into this document.
