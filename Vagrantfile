# Copyright 2015, RackN
# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
#
# Needs vagrant-triggers
# vagrant plugin install vagrant-triggers
#
# Make sure that vagrant is around 1.7+
# Make sure virtualbox is 5.0+
#
# For Linux:
# https://www.virtualbox.org/wiki/Linux_Downloads
# http://www.vagrantup.com/downloads
#
# Be sure to bridge the vboxnet0 into docker0
# brctl addif docker0 vboxnet0
# Run the container in FORWARDER mode
#
VAGRANTFILE_API_VERSION = "2"
#BASE_OS_BOX = "ubuntu/trusty64"
BASE_OS_BOX = "bento/centos-7.1"
SLAVE_RAM = "2048"
# Host Mode - MAC
ADMIN_PREFIX = "192.168.99"
ADMIN_IP = "#{ADMIN_PREFIX}.100"
ADMIN_CIDR = 24
#
# Forwarder Mode - Linux
#ADMIN_PREFIX = "192.168.124"
#ADMIN_IP = "#{ADMIN_PREFIX}.11"
#ADMIN_CIDR = 24

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|

  puts "======================="
  puts "WARNING > EXPERIMENTAL!"
  puts "======================="

  config.vm.define "admin", autostart:false do |admin|

    admin.vm.box = BASE_OS_BOX

    # Create a private network, which allows host-only access to the machine
    # using a specific IP.
    admin.vm.network "private_network", ip: ADMIN_IP, auto_config: true

    # avoid redownloading large files      
    FileUtils.mkdir_p "#{ENV['HOME']}/.cache/digitalrebar/tftpboot"
    admin.vm.synced_folder "#{ENV['HOME']}/.cache/digitalrebar/tftpboot",
          "/root/.cache/digitalrebar/tftpboot",
          type: 'nfs', nfs_udp: false,
          bsd__nfs_options: [ 'maproot=root:wheel' ],
          linux__nfs_options: [ 'maproot=root:wheel' ]

    admin.vm.provider "virtualbox" do |vb|
      vb.memory = "4096"
      vb.cpus = 4
    end

    #
    # Admin nodes eat themselves without swap
    #
    admin.vm.provision "shell", path: "scripts/increase_swap.sh"

    admin.vm.provision "ansible" do |ansible|
      ansible.sudo = true
      ansible.sudo_user = "root"
      ansible.playbook = "digitalrebar.yml"
    end

    puts "To monitor > https://#{ADMIN_IP}:3000 (Digital Rebar)"
    puts "After the system is up, you can start the nodes using `vagrant up /node[1-3]/`"
  end

  (1..20).each do |i|
    config.vm.define "node#{i}", autostart:false do |slave|
      slave.vm.hostname = "node#{i}.rebar.local"
      slave.vm.box = BASE_OS_BOX
      slave.vm.network "private_network", ip: "#{ADMIN_PREFIX}.#{200+i}", auto_config: true
      slave.vm.provider "virtualbox" do |vb|
        vb.memory = SLAVE_RAM
      end
      slave.vm.provision "shell", path: "scripts/join_rebar.sh", args: "#{ADMIN_IP} #{ADMIN_PREFIX}.#{200+i}/#{ADMIN_CIDR}"
      slave.trigger.after :destroy do
        run "rebar -E https://#{ADMIN_IP}:3000 -U rebar -P rebar1 nodes destroy node#{i}.rebar.local"
      end
    end
  end

end
