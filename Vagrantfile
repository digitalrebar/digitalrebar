# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
    config.vm.box = "ubuntu/trusty64"
	# config.vm.box = "parallels/ubuntu-14.04 "

    # Create a private network, which allows host-only access to the machine
    # using a specific IP.
    config.vm.network "private_network", ip: "192.168.124.10", auto_config: true
    config.vm.network "private_network", ip: "10.10.10.10", auto_config: false

    config.vm.network "forwarded_port", guest: 3000, host: 3030
    config.vm.network "forwarded_port", guest: 8500, host: 8585
    
    config.vm.provision "ansible" do |ansible|

	    # proxy pass through
	    if Vagrant.has_plugin?("vagrant-proxyconf") && ENV['http_proxy'] 
	      port = ENV['http_proxy'].split(":")[2]
	      config.proxy.http     = "http://10.0.2.2:#{port}"
	      config.proxy.https    = "http://10.0.2.2:#{port}" 
	      config.proxy.no_proxy = "127.0.0.1,[::1],localhost,192.168.124.0/24,192.168.1.0/12,10.0.2.0/24"
	    else
	      puts "If you have a local proxy cache, install the proxy plug-in! 'vagrant plugin install vagrant-proxyconf'"
	    end

		ansible.sudo = true
		ansible.sudo_user = "root"
        ansible.playbook = "ubuntu1404.yml"
    end
end
