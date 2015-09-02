# Using the Ansible Playbook

The following documentation will setup a complete running Digital Rebar system for test purposes.

Available playbooks:
  * ubuntu1404.yml

## Prereqs

You need a target system:

  * with a matching operating system installed (see Available playbooks above)
  * root user access via your local SSH key
  * internet connectivity from the system (faster is better)
  * 4 Gb of RAM (more if you want to run slaves on the system)

On your local workstation:

  * Ansible installed (`sudo apt-get install ansible`)
  * IP of the target system in Ansible inventory (`sudo vi /etc/ansible/hosts`)
    * include `ansible_ssh_user=root` after the IP

## Install Process

> There is no need to connect to the remote system via SSH.  All steps are performed from your workstation by Ansibile.

From your workstation:

  * run the selected playbook: `ansible-playbook [ubuntu1404.yml]`
    * include `-u root` if you did _not_ specific an ssh user in inventory
  * watch and wait
  * visit the Digital Rebar UI: http://[system address]:3000

Note: It will take additional time after the playbook completes before Digital Rebar is available.

### What's installed?

  * `vagrant` user
    * same authorized keys as root user
    * passwordless SUDO
  * all pre-requists including latest Docker with correct permissions
  * latest Digital Rebar code from develop branch
  * latest Kubernetes workload from RackN
  * Node target operating systems:
    * Ubuntu 14.04 ISO
  * Default IP maps for Digital Rebar: 
    * internal address of 192.168.124.10
    * port mapping of UI (:3000), Consul (:8500) and Chef (:443)
  * IP mapping of host as 192.168.124.4 (so you can ssh/ping Admin container)

## Next Steps

### Spinning up nodes (aka slaves)

Once you have the Digital Rebar admin running, you can add nodes into the environment.  At this time, you must _SSH to the system_ to complete these steps.

*SSH as the `vagrant` user*

From the `~/core` directory

  * Start: `for j in 1 2 3; do tools/kvm-slave & done`
  * Stop: `for j in 1 2 3; do kill %$j ; done`

See [kvm slaves](https://github.com/digitalrebar/doc/blob/master/development/advanced-install/kvm-slaves.rst) doc

### Attaching to networks (to boot metal)

> Select the interface you want to use for DHCP/Discovery.

`sudo brtcl addif docker0 [external interface]`

### Update the Digital Rebar code:

  * Find the Admin container: `docker ps`
  * Kill the Admin container: `docker kill [container id]`
  * Update the code in core: `cd core` then `git pull`
  * Restart the container: `home/vagrant/core/tools/docker-admin --daemon centos ./production.sh admin.rebar.digital`
