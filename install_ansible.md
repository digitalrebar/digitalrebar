# Using the [Ansible](http://ansible.com) Playbook

The following documentation will setup a complete running Digital Rebar system for test purposes.

*PRO TIP:* Playbooks can be _localhost_ applied to run local setups!

Running ansible local:
  * echo "127.0.0.1" > run-in-hosts
  * ansible-playbook -i run-in-hosts digitalrebar.yml --connection=local

Available playbooks:
  * [digitalrebar.yml](digitalrebar.yml) generic install based on Docker Compose
  * Supporting playbooks
    * [docker.yml](tasks/docker.yml) Installs Docker & Compose
    * [ubuntu1404-base.yml](tasks/ubuntu1404-base.yml) Ubuntu Pre-reqs
    * [centos07-base.yml](tasks/centos07-base.yml) Centos Pre-reqs
    * [packet.yml](packet.yml) Added optimizations for Packet.net

## Prereqs

You need a target system:

  * with a matching operating system installed (see Available playbooks above)
  * root user access via your local SSH key
  * internet connectivity from the system (faster is better)
  * 4 Gb of RAM (more if you want to run slaves on the system)

On your local workstation:

  * Clone the [Digital Rebar Deploy](https://github.com/rackn/digitalrebar-deploy.git) repo - this one!
    * Use `compose/setup.sh` to do this automatically
    * checkout out additional workloads (`compose/setup.sh rackn` will get all)
  * Ansible installed
    *  Ubuntu (`sudo apt-get install ansible`)
    *  Centos (`yum install ansible`)
      * May have to add epel first (`yum install epel-release`)
  * IP of the target system in Ansible inventory (`sudo vi /etc/ansible/hosts`)
    * include `ansible_ssh_user=root` after the IP
    * If doing localhost, make sure to use the IP of the node and not 127.0.0.1

## Install Process

> There is no need to connect to the remote system via SSH.  All steps are performed from your workstation by Ansible.

From your workstation:

  * run the selected playbook: `ansible-playbook [digitalrebar.yml]`
    * include `-u root` if you did _not_ specific an ssh user in inventory
  * watch and wait
  * visit the Digital Rebar UI: http://[system address]:3000

Note: It will take additional time after the playbook completes before Digital Rebar is available.

### What's installed?

  * all pre-requists including latest Docker with correct permissions
  * latest Digital Rebar code from develop branch
  * Node target operating systems:
    * Ubuntu 14.04 and Centos 7 ISO
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
