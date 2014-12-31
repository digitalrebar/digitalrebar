# Ubuntu with KVM 
The steps here describe how to setup the VM from the command line on Ubuntu. You can
use [virt-manager](http://virt-manager.org) if you prefer a graphical user
interface. Do submit your relevant virt-manager configs if you have some!

The steps here assume that your KVM host is also the desktop that you are
working from. If not, adapt the commands accordingly.

Installation steps:

1. Download Ubuntu 12.04 LTS 64 bit (`ubuntu-12.04.1-server-amd64.iso`) from
   http://www.ubuntu.com/download/server. For example, run the following
   commands within the OpenCrowbar git checkout on the qemu-kvm host:

   ````
   cd dev-setup/qemu-kvm
   aria2c http://releases.ubuntu.com/precise/ubuntu-12.04.1-server-amd64.iso.torrent
   ````

1. Create a blank disk image that is at least 20 GB. For example:

   ````
   qemu-img create -f qcow2 -o preallocation=metadata ubuntu-12.04.qcow2 20G
   ````

1. Start a VM with the desired network (private network with NAT), with the ISO
   and disk attached. For example:

   ````
   sudo qemu-kvm -m 2G -daemonize -vnc :10 -cdrom ubuntu-12.04.1-server-amd64.iso \
                 -net nic,model=virtio,macaddr=DE:AD:BE:EF:30:22 \
                 -net tap,script=qemu-ifup \
                 -drive file=ubuntu-12.04.qcow2,cache=none,if=virtio
   ````

   Note that `script=qemu-ifup` points to the script at `qemu-kvm/qemu-ifup`,
   so make sure you are running the above command in the same directory, or
   modify it accordingly.

1. Connect to the VM via VNC and install the system:

   ````
   vncviewer :10
   ````

   The installer will attempt to auto-configure the network with DHCP, which
   you can cancel and jump to manual configuration instead with the following
   settings:

   ````
   IP address: 192.168.124.10
   Netmask:    255.255.255.0
   Gateway:    192.168.124.1
   ````

   Use the same name server (DNS) address as your host, which you can find out
   on Linux systems by running `grep nameserver /etc/resolv.conf` on the host.
   For example, within the SUSE intranet it is `10.120.2.88`. If the host is not
   running in any internal or corporate network, you can use `8.8.8.8`.

   The hostname and domain names can be left at the defaults. The apt-get proxy
   can also be left blank.

1. Once installation is complete, you can shutdown the VM (`sudo poweroff`) and
   subsequently start it in the same way, minus the `-cdrom ...` option. Or
   use the [qemu-kvm/start-vm](https://github.com/crowbar/crowbar/blob/master/dev-setup/qemu-kvm/start-vm))
   helper script.
