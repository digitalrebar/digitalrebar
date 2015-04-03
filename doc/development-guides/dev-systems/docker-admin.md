## Admin Node in Docker

It is possible (and convienent) to run a OpenCrowbar admin node in a
CentOS 6.5 based Docker container.  To do so, you need to be running
in a development environment that can run Docker.  

> Once the admin node is running in a container, it will keep running until you kill the container using `docker kill [cid]` or `exit` from the container prompt.  We recommend looking at the Docker commands for additional options.

### Install Docker

Instructions for installing Docker on the most common Linux distributions are at
[http://docs.docker.io/en/latest/installation/]

> DO NOT TEST docker until you follow the steps below!

### Configure Docker in your development environment

We assume you have given yourself permission to run docker without sudo.  
To do this, add your user to the docker group.

  *  `sudo usermod -a -G docker <your-user>` (to permanently run Docker
  without sudo)
  * you will need to reboot after this change (but you can wait until we tell you to reboot later)

> if you don't want this to be permanent or active before the reboot use, `sudo chmod 666 /var/run/docker.sock`

Configure docker to use the
devicemapper storage backend and to talk through your HTTP proxy (if
any)  We need to use the devicemapper storage backend because there
are directory permissions bugs in the AUFS driver that our CentOS
container exposes.

On Ubuntu, edit `/etc/default/docker` and make the following changes:

  * Uncomment the line that starts with `DOCKER_OPTS`, and make it read
  `DOCKER_OPTS="-s devicemapper"`
  * If you need to have the Docker daemon talk through an http proxy,
  uncomment the line that starts with `export` and change the part after
  `http_proxy` to point at the http proxy you normally use.

On CentOS 6.5, edit `/etc/sysconfig/docker`, and make the following
changes:

  * Change the line that starts with `other_args` to read
  `other_args="-s devicemapper"`.
  * Add a line that reads `export http_proxy="http://<your_http_proxy>"`
    if you need to have the Docker daemon talk through an http proxy.
  * If you need a proxy to talk https, add a similar line reading
  `export https_proxy="http://<your_https_proxy>"`

On OpenSuSE 13.1, Fedora 20, and other distributions that use systemd
as their init system, perform the following steps:

  1. Copy `/usr/lib/systemd/system/docker.service` to
  `/etc/systemd/system/docker.service`

  2. Edit `/etc/systemd/system/docker.service`, and make the following
  changes:

    * Change the line that starts with `ExecStart=` and append
    ` -s devicemapper` to the end of it.
    * If you need to have the Docker daemon talk through an http proxy,
    add the following line directly under the `[Service]` line:

      `Environment="http_proxy=http://<your_http_proxy>" "https_proxy=http://<your_http_proxy>"`

  3. Reload the docker service configuration: `systemctl daemon-reload`

  After making the above changes. reboot or restart the Docker service (`sudo service docker restart`) for them to
  take effect.

### The docker-admin command and its environment

The `docker-admin` command (located in the `tools` directory in the
core repository) is responsible for managing the interaction between
the development environment and the Docker container.  Among other
things, it ensures that:

* The contents of the `core` repository in the development environment
is visible in the Docker container at `/opt/opencrowbar/core`.  This
makes it trivial to edit the code in your development environment and
have the changes be instantly visible in the Docker container.
* The contents of `$HOME/.cache/opencrowbar/tftpboot` is visible in
  the Docker container at `/tftpboot`.  This keeps the Docker
  container from getting too bloated when setting up parts of the
  provisioner.
* The UID and GID of the OpenCrowbar user in the container are identical
to your UID and GID in your development environment.
* Your SSH public key in your development environment is added to
`/root/.ssh/authorized_keys`
* Your `http_proxy`, `https_proxy` and `no_proxy` environment
  variables will be visible in the Docker container.  If your
  `http_proxy` and `https_proxy` environment variables refer to
  `localhost`, `127.0.0.1`, or `[::1]`, then they will be rewritten to refer
  to the IP address of the bridge that Docker is using.  In that case,
  your local proxy should be configured to allow connections from
  `172.16.0.0/12`.

### Ensuring that the admin node can deploy operating systems to slaves

> More complete instructions in (Deployment Guide)[../../deployment-guide/adding-operating-systems.md]

When deploying an admin node in production mode, you will want to be
able to install operating systems on slave nodes.  By default, the
`provisioner-base-images` role will look for OS install ISO images in
`/tftpboot/isos`.  The provisioner knows how to install several operating systems 
(partial list below) from the ISO images:

 * `ubuntu-14.04`: `ubuntu-14.04.1-server-amd64.iso`
 * `centos-6.6`: `CentOS-6.6-x86_64-bin-DVD1.iso`
 * `centos-7.1.1503`: `CentOS-7-x86_64-DVD-1503.iso`

> This list is subject to change!  For the latest list, consult [Provisioner Base Images](https://github.com/opencrowbar/core/blob/master/chef/roles/provisioner-base-images/role-template.json) template file.

To enable the provisioner to install from those images, place them in
`$HOME/.cache/opencrowbar/tftpboot/isos`, either directly or via a
hard link (soft links do not work for Docker).  These images will then be available inside the Docker
container at `/tftpboot/isos`, and the provisioner will be able to use
them to install operating systems on slave nodes.

If you do this AFTER the admin node is running, you must rerun the Provisioner OS Repos role.

### Running a production mode OpenCrowbar admin node in Docker

Once Docker is installed, configured, and you have ISO images in
place, you are ready to run a OpenCrowbar admin node on CentOS 6.5 in
Docker.  To do that, run the following command from the core
repository:

    tools/docker-admin centos ./production.sh admin.smoke.test

> note: the crowbar-bootstrap step takes a while, be patient

This will perform the following actions:

  * If needed, pull the latest opencrowbar/centos image from the public
  Docker repository.
  * Spawn the container with all the parameters needed to set up the
  environment as described above.  The rest of the actions will take
  place in the spawned container.
  * Ensure that the UID and GIDs of crowbar user inside the container is
    the same as your UID and GID in the development environment.
  * Append your SSH public key to root's authorized_keys file.
  * Run `./bootstrap.sh`, which will ensure that ruby and chef-solo are
  installed, and then run the crowbar-bootstrap cookbook to converge the
  state of the container with our latest specifications.
  * Bring up the OpenCrowbar webserver.
  * Create a default admin network on the `192.168.124.0/24` address
  range.
  * Update the `provisioner-server` role template to use the passed-in
  http proxy, if any.
  * Update the `provisioner-os-install` role template to default to
  `centos-7.1.1503`.
  * Create the admin node record.
  * Extract the addresses that were allocated to the admin node, and
  bind them to eth0.
  * Mark the admin node as alive, and converge the default set of admin
  noderoles.
  * You can turn off the TMUX launching using `export TMUX=false`

Options:

  * --zombie will run all of the admin config except for the final "node alive" step.  This is handy if you want to check the system before completes

You should be able to monitor the progress of the admin node
deployment at http://localhost:3000.  Once the admin node is finished
deploying (or if anything goes wrong), you will be left at a running
shell inside the container.

You can ssh into the container from the host by finding its IP address
through Docker, as below. The container should already have your ssh 
keys copied into the proper place.


```
$ docker ps
CONTAINER ID        IMAGE                       COMMAND                CREATED             STATUS              PORTS                                          NAMES
0db77a80acd0        opencrowbar/centos:6.5-11   "/opt/opencrowbar/co   32 minutes ago      Up 32 minutes       0.0.0.0:443->443/tcp, 0.0.0.0:3000->3000/tcp   evil_bohr           
$ docker inspect 0db77a80acd0 | grep IPAddress
        "IPAddress": "172.17.0.7",
$ ssh root@172.17.0.7
Last login: Wed Aug 27 16:20:41 2014 from 172.17.42.1
[root@0db77a80acd0 ~]# 
```


### Booting slave VMs from the admin node

Prereq : `sudo apt-get install bridge-utils`


#### Bare Metal (the easy way)
If your development environment is running on bare metal (as opposed
to running inside a VM), you can use `tools/kvm-slave &` to spawn a
KVM virtual machine that will boot from the freshly-deployed
admin node.

#### Real Hardware slaves

To boot Real Hardware, bind a physical interface to docker0 with brctl, make sure that interface is up and does not have an address, and plug it in to a switch that has the physical boxes you want to boot.

Example Commands:
  1. slave the eth2 to the docker bridge, `sudo brctl addif docker0 eth2`
  1. turn on eth2 for the bridge, `sudo ip link set eth2 up`
  1. boot the physical nodes from a switch connected to eth2

#### Virtual Box (the corporate way)

> This approach expects that you've added an ethernet device (not up'd) to your VM that will be the admin network for slave VMs. Also, if using vmware, you'll need to use E1000 Nics and make sure your network settings are set to "Allow" promiscuous mode. 

If your development environment is running in VMs then:

  1. make sure that your dev VM has an extra eth port connected to a dedicated host only bridge (let's assume eth2)
  1. slave the eth2 to the docker bridge, `sudo brctl addif docker0 eth2`
  1. turn on eth2 for the bridge, `sudo ip link set eth2 up`
  1. create a VM with eth0 
    1. attached to the dedicated host only bridge 
    1. make sure it is able to network boot
  1. boot the VM
    1. it should PXE boot
    1. the VM should register and automatically progress in the system deployment
    1. if you have issues, review the `/var/log/install.log` for details

### Development Admin

  1. Dev/Simulator allows you to play with the UI and BDD tests which is good for developers working on the UI/API and Annealer logic
    1. (optionally) Disable TMUX mode using `export TMUX=false`
    1. Start with `tools/docker-admin centos ./development.sh`
    1. Dev mode creates a special user `developer/Cr0wbar!`
    1. To monitor the logs inside the container, use `tail -f /var/log/crowbar/development.log`
    1. Run the BDD system [see BDD test pages]
       1. `sudo apt-get install erlang`
       1. compile the BDD code
       1. update the config file (copy example.config to default.config and update)
       1. `erl` then `bdd:test()`
    1. Rails console in container: `su -l -c 'cd /opt/opencrowbar/core/rails; bundle exec rails c' crowbar`
'
