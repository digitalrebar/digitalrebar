# Copyright 2015, RackN
# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
#
# Needs vagrant-triggers
# vagrant plugin install vagrant-triggers
#
VAGRANTFILE_API_VERSION = "2"
#BASE_OS_BOX = "ubuntu/trusty64"
BASE_OS_BOX = "bento/centos-7.1"
SLAVE_RAM = "2048"
ADMIN_PREFIX = "192.168.99"
ADMIN_IP = "#{ADMIN_PREFIX}.100"

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

  config.vm.define "node1", autostart:false do |slave|
    slave.vm.hostname = "node1.rebar.local"
    slave.vm.box = BASE_OS_BOX
    slave.vm.network "private_network", ip: "#{ADMIN_PREFIX}.201", auto_config: true
    slave.vm.provider "virtualbox" do |vb|
      vb.memory = SLAVE_RAM
    end
    slave.vm.provision "shell", path: "scripts/join_rebar.sh", args: "#{ADMIN_IP} #{ADMIN_PREFIX}.201"
    slave.trigger.after :destroy do
      run "rebar -E https://#{ADMIN_IP}:3000 -U rebar -P rebar1 nodes destroy node1.rebar.local"
    end
  end

  config.vm.define "node2", autostart:false do |slave|
    slave.vm.hostname = "node2.rebar.local"
    slave.vm.box = BASE_OS_BOX
    slave.vm.network "private_network", ip: "#{ADMIN_PREFIX}.202", auto_config: true
    slave.vm.provider "virtualbox" do |vb|
      vb.memory = SLAVE_RAM
    end
    slave.vm.provision "shell", path: "scripts/join_rebar.sh", args: "#{ADMIN_IP} #{ADMIN_PREFIX}.202"
    slave.trigger.after :destroy do
      run "rebar -E https://#{ADMIN_IP}:3000 -U rebar -P rebar1 nodes destroy node2.rebar.local"
    end
  end

  config.vm.define "node3", autostart:false do |slave|
    slave.vm.hostname = "node3.rebar.local"
    slave.vm.box = BASE_OS_BOX
    slave.vm.network "private_network", ip: "#{ADMIN_PREFIX}.203", auto_config: true
    slave.vm.provider "virtualbox" do |vb|
      vb.memory = SLAVE_RAM
    end
    slave.vm.provision "shell", path: "scripts/join_rebar.sh", args: "#{ADMIN_IP} #{ADMIN_PREFIX}.203"
    slave.trigger.after :destroy do
      run "rebar -E https://#{ADMIN_IP}:3000 -U rebar -P rebar1 nodes destroy node3.rebar.local"
    end
  end

end
