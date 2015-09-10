# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"
BASE_OS_BOX = "ubuntu/trusty64"
SLAVE_RAM = "2048"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|

  config.vm.define "admin", primary: true do |admin|
  
    admin.vm.box = BASE_OS_BOX

    # Create a private network, which allows host-only access to the machine
    # using a specific IP.
    admin.vm.network "private_network", ip: "192.168.124.10", auto_config: true
    admin.vm.network "private_network", ip: "10.10.10.10", auto_config: false

    #admin.vm.network "forwarded_port", guest: 3000, host: 3030
    #admin.vm.network "forwarded_port", guest: 8500, host: 8585

    # avoid redownloading large files      
    admin.vm.synced_folder "~/.cache/digitalrebar/tftpboot/isos/", "/home/vagrant/.cache/digitalrebar/tftpboot/isos/"

    admin.vm.provision "ansible" do |ansible|
  		ansible.sudo = true
  		ansible.sudo_user = "root"
      ansible.playbook = "ubuntu1404.yml"
    end

    puts "To monitor http://192.168.124.10:8500 (Consul) andhttp://192.168.124.10:3000 (Digital Rebar)"
    puts "After the system is up, you can start the nodes using `vagrant up /node[1-3]/`"

  end

  config.vm.define "node1", autostart:false do |slave|

    slave.vm.box = BASE_OS_BOX
    slave.vm.network "private_network", ip: "192.168.124.101", auto_config: true
    slave.vm.network "private_network",  ip: "10.10.10.101", auto_config: false
    slave.vm.provider "virtualbox" do |vb| 
      vb.memory = SLAVE_RAM
    end
  end

  config.vm.define "node2", autostart:false do |slave|

    slave.vm.box = BASE_OS_BOX
    slave.vm.network "private_network", ip: "192.168.124.102", auto_config: true
    slave.vm.network "private_network",  ip: "10.10.10.102", auto_config: false
    slave.vm.provider "virtualbox" do |vb| 
      vb.memory = SLAVE_RAM
    end
  end

  config.vm.define "node3", autostart:false do |slave|

    slave.vm.box = BASE_OS_BOX
    slave.vm.network "private_network", ip: "192.168.124.103", auto_config: true
    slave.vm.network "private_network",  ip: "10.10.10.103", auto_config: false
    slave.vm.provider "virtualbox" do |vb| 
      vb.memory = SLAVE_RAM
    end
  end

end
