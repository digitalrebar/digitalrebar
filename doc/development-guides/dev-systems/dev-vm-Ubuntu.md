# THIS IS DEPRECATED.  Use the Docker based development environment instead [here](README.md)

# OpenCrowbar Development Environment Based Upon Ubuntu

## Setting Up the Virtual Machine (VM)

Currently only Ubuntu 12.04 LTS is supported, though the instructions here
should also work with other versions.

The steps here describe how to setup the VM from the command line. You can
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

### Setup password-less sudo
During the build process the Dev Tool has to perform certain tasks which require root access (mounting ISOs, etc.). In order to avoid being prompted for your password every time we will setup password-less sudo. **Don't run your build as root.**

    # run this command to add your 
    sed -ie "s/%sudo\tALL=(ALL:ALL) ALL/%sudo ALL=(ALL) NOPASSWD: ALL/g" /etc/sudoers

## Setting up the development environment

Before beginning to set up a development environment, you should have
a working VM that with a build user created.  These directions use 'crowbar'
as the build user.

The general requirements are:

1. You should be able to access the machine from the host with ssh as the build 
   user.
1. The build user has passwordless sudo access enabled.
1. The machine has outbound acess via http for downloading packages,operating 
   system images, and Ruby gems, including any necessary firewall and 
   proxy setup.
1. The machine has access to github for fetching code.
1. The machine has approximately 40Gb of free disk space 
   ( 15 Gb - operating system images, 10 Gb for output isos, 15 Gb for 
   build cache

After verifying these requirements, you can begin setting up the development 
environment.

### Install needed packages and gems 

These directions are for Ubuntu 12.04 (Precise.) Other versions of
Ubuntu are not supported.  Postgresql is only supported by the Postgresql
community on LTS releases.

    # let's install some OS packages
    sudo apt-get update
    sudo apt-get install git rpm ruby ruby1.9.1-dev rubygems1.9 curl build-essential debootstrap \
    mkisofs binutils markdown erlang-base debhelper python-pip libsqlite-dev \
     libopenssl-ruby1.9.1 libssl-dev zlib1g-dev ruby-sqlite3 libsqlite3-dev
    sudo apt-get install libpq-dev
    # to make Ruby 1.9.1 the default. ruby -v will report version 1.9.3
    sudo update-alternatives --config ruby 
    # make Gem 1.9 the default, gem -v will report version 1.8.11
    sudo update-alternatives --config gem 
    #
    # Remove Postgresql
    #
    # we need Postgresql 9.3 (we rely on 9.3+ features)
    # first, remove the automatically added old Posgresql
    sudo apt-get remove postgresql
    # To Verify that you have removed postgresql you can run
    sudo dpkg --get-selections | grep postgresql
    # if there is anything still there with deinstall do a
    sudo dpkg --purge postgres* 
    #
    #
    # Additional reference, please visit [[https://wiki.postgresql.org/wiki/Apt]]
    # for now you need to add the sources (please remove this step when 9.3 is in the official repos!)
    # You will need to edit /etc/apt/sources.list and add the following to it.
    # Add -     deb http://apt.postgresql.org/pub/repos/apt/ [your release]-pgdg main
    # where [your release] is the version of OS you using, i.e. Ubunutu-precise is "precise-pgdg" (without the quotes)
    wget --quiet -O - http://apt.postgresql.org/pub/repos/apt/ACCC4CF8.asc | sudo apt-key add -
    sudo apt-get update
    # now install and set to use the special port/pipe config
    sudo apt-get install postgresql-9.3 pgadmin3
    sudo vi /etc/postgresql/9.3/main/pg_hba.conf
      # to the beginning of the file 
      # add 'local  all   all    trust' 
    sudo vi /etc/postgresql/9.3/main/postgresql.conf
      # change 'port = 5439'
    sudo service postgresql restart
    sudo createuser -s -d -U postgres -p 5439 crowbar
    # you can test the install by making sure the following call returns
    export PGCLUSTER=9.3/main
    psql postgresql://crowbar@:5439/template1 -c 'select true;'

    # let's install some needed gems next
    sudo gem install builder bluecloth
    sudo gem install bundler --version '1.3.5' --no-ri --no-rdoc
    sudo gem install json net-http-digest_auth kwalify delayed_job delayed_job_active_record rake simplecov rspec pg --no-ri --no-rdoc

