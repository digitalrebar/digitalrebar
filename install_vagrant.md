# [Vagrant](https://www.vagrantup.com/) Install of Digital Rebar

*WARNING: EXPERIMENTAL*

The following documentation will setup a complete running Digital Rebar admin node for test purposes.

> Behind the scenes, this process uses an [Ansible playbook](install_ansible.md) install.

> You cannot run a local docker-compose instance of Digital Rebar and do Vagrant installs at the same time.

## Prereqs

On your local workstation:

  * Vagrant installed (from [Vagrant website](http://www.vagrantup.com/downloads.html) not apt-get)
  * Vagrant requires several other items:
     * Virtualbox (or similar) (e.g.: `sudo apt-get install virtualbox`)
     * Ansible plugin: `vagrant plugin install ansible`
     * (optional if you have a proxy) Vagrant Proxy: `vagrant plugin install vagrant-proxyconf`
     * (optional) Pre-fetch the box: `vagrant box add ubuntu/trusty64`

## Install Process

From your workstation:

  * put the `Vagrantfile` and `ubuntu1404.yml` files in a directory (cloning this repo is recommended)
  * go to that directory
  * start Vagrant: `vagrant up`
  * watch and wait (could take a long time depending on your system and connection)
  * you can attach to the Admin container using SSH root@192.168.124.10 or with `docker attach [id]` from the vagrant box (use `vagrant ssh`) 

Note: It will take additional time after the playbook completes before Digital Rebar is available.

### What's installed?

  * `vagrant` user
    * password is `vagrant`
    * passwordless SUDO
  * all pre-requists including latest Docker with correct permissions
  * latest Digital Rebar code from develop branch
  * latest Kubernetes workload from RackN
  * Node target operating systems:
    * Ubuntu 14.04 ISO
  * Default IP maps for Digital Rebar: 
    * internal address of 192.168.124.10
    * port mapping of UI (:3000), Consul (:8500) and Chef (:443)
  * IP mapping of host as 192.168.124.10 (so you can ssh/ping Admin container)

## Next Steps

### Spinning up nodes (aka slaves)

_Not supported at this time._

### Attaching to networks (to boot metal)

_Not supported at this time._

### Update the Digital Rebar code:

  * Find the Admin container: `docker ps`
  * Kill the Admin container: `docker kill [container id]`
  * Update the code in core: `cd core` then `git pull`
  * Restart the container: `home/vagrant/core/tools/docker-admin --daemon centos ./production.sh admin.rebar.digital`

### Cleanup 

When finished, you can destroy the system using `vagrant destroy`

