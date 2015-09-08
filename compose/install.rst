Digital Rebar Install
=====================

*Approximate install time: 15 minutes depending on bandwidth.*

The install steps are:

1. Install Docker & Docker-Compose
#. Download code & prerequists (e.g.: operating systems to install)
#. Start infrastructure containers
#. Provision nodes! (just testing? use fast virtual nodes)

Rather than cover every operating system, we are assuming that you can translate between differences in major distributions.

*Need help?* Jump over to our `live chat <https://gitter.im/digitalrebar/core>`_  (Gitter.im)

**RECOMMENDATION:** Review the `RackN maintained deploy scripts <https://github.com/rackn/digitalrebar-deploy>`_ for updated step-by-step install examples.

Admin In Containers!
--------------------

The goal for this document is getting a basic Digital Rebar Infastructure running quickly.  For that reason, many options and configuration choices have been omitted in the interest in brevity.

Digital Rebar operates all the infrastructure management functions in Docker containers; consequently, you need to be running in an environment that can run Docker.

    To improve support, the `Digital Rebar team <https://github.com/orgs/digitalrebar/teams>`_ is no longer creating or documenting install packages.

    For developers, we've collected some `additional guidance <development/advanced-install>`_ to review after you've got your first install working.

We are going to assume that you know how to use basic Docker and Docker Compose commands to keep these instructions concise.

Step 1. Install Docker & Docker Compose
---------------------------------------

Follow the `Docker install guide <http://docs.docker.io/en/latest/installation/>`_

- Install Docker. [8]_
- Get permission to run Docker without sudo. [1]_
- Turn off Apparmor [2]_ (`production deploy <deployment/>`_ could be configured to leave on)
- Map a local address (192.168.124.10/24) to the docker bridge. [7]_

Follow the `Compose install guide <https://docs.docker.com/compose/install/>`_ 

Step 2. Download Code & Prerequists
-----------------------------------

These steps are for **default** configuration.  Advanced configurations can adapt to more complex environments.

- Make sure you have *disabled* the following services locally:
   - bind: DNS server on :53 (e.g.: ``killall dnsmasq``)
   - proxy: local proxy on :8123 (e.g.: ``service squid3 stop``) 
   - db: PostgreSQL on :5432
   - rails: local web apps on :3000
   - when starting Compose, you will be alerted if there are other `assigned port conflicts <docker-compose-common.yml>`_ .
- Make sure you have an ssh key [4]_
- (optional) Enable passwordless sudo [5]_
- Download at least one ISO from the list in `provisioner.yml <https://github.com/digitalrebar/core/blob/develop/barclamps/provisioner.yml#L135>`_ and copy to ``~/.cache/opencrowbar/tftpboot/isos``
- Install git.
- Clone RackN Digital Rebar Compose: ``git clone https://https://github.com/rackn/container-install.git compose``
- Run the ``./setup`` command in that repo

Step 3. Deploy infrastructure containers
----------------------------------------

From the Compose directory, run ``docker-compose up -d`` to start the process.

You can monitor the progress in several ways:

#. Starting Compose without the ``-d`` flag will send logs to the screen.  In this mode, we suggest grepping the contents to eliminate logstash.  [9]_ 
#. The Digital Rebar Consul service comes up quickly on http://127.0.0.1:8500
#. A Kibana logstash service is running on http://127.0.0.1:5601
#. ``docker-compose ps`` will show you the status of the services and associated port mappings.
#. ``docker ps`` will show you the status of the containers

To reset the environment, you must stop [10]_ and then remove [11]_ the containers.

After the install has progressed (the ``drebar-api-service`` is up in `Consul <http://127.0.0.1:8500>`_ ), you should be able to monitor the progress of the Admin container at http://localhost:3000.

You can connect to any container using ``docker exec -it [name] bash``; however, we recommend using `Kibana <http://127.0.0.1:5601>`_ to check centralized logs first.

Note: you can start in development mode by starting Compose with the ``-f development.yml`` flag

Step 4. Provision Nodes!
------------------------

And now, the real fun begins!  

If this is your first install, the Docker and KVM nodes approach will allow you to play with Digital Rebar with minimal network configuration.

Docker Nodes (fast testing)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

From the deploy directory:

#. ``docker-compose scale node=5``

You can turn the number of nodes up and down by changing the number.

KVM Nodes (high fidelity test)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note: this only works on Linux environments that can run KVM.

#. Install prereqs: 

   #. ``apt-get install qemu-kvm libvirt-bin ubuntu-vm-builder bridge-utils ruby1.9.1-dev make``
   #. ``gem install json net-http-digest_auth``

#. Use ``tools/kvm-slave &`` to spawn a KVM virtual machine that will boot from the freshly-deployed admin node.

More details? See `virtual nodes <development/advanced-install/kvm-slaves.rst>`_ for testing using KVM.

Real Hardware
~~~~~~~~~~~~~

To boot Real Hardware, bind a physical interface to docker0 with brctl,
make sure that interface is up and does not have an address, and plug it
in to a switch that has the physical boxes you want to boot.

Example Commands: 1. slave the eth2 to the docker bridge,
``sudo brctl addif docker0 eth2`` 1. turn on eth2 for the bridge,
``sudo ip link set eth2 up`` 1. boot the physical nodes from a switch
connected to eth2

Virtual Box (generally for Windows users)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    This approach simulates the same steps as metal, so it expects that you've created a VM to host the
    Admin container.  If so, make sure you added an ethernet device (not
    up'd) to your VM that will be the admin network for slave VMs. Also,
    if using vmware, you'll need to use E1000 Nics and make sure your
    network settings are set to "Allow" promiscuous mode.

If your development environment is running in VMs then:

#. make sure that your Admin VM has an extra eth port connected to a
   dedicated host only bridge (let's assume eth2)
#. slave the eth2 to the docker bridge,
   ``sudo brctl addif docker0 eth2``
#. turn on eth2 for the bridge, ``sudo ip link set eth2 up``
#. create a VM with eth0

   #. attached to the dedicated host only bridge
   #. make sure it is able to network boot

#. boot the VM

   #. it should PXE boot
   #. the VM should register and automatically progress in the system
      deployment
   #. if you have issues, review the ``/var/log/install.log`` for
      details

Additional References
---------------------

**WARNING**: These suggestions may become out of date.  We strongly recommend reviewing the actively maintained `deploy scripts <https://github.com/rackn/digitalrebar-deploy>`_.

.. [1] ``sudo usermod -a -G docker <your-user>``
   plus, if you don't want to reboot, run ``sudo chmod 666 /var/run/docker.sock``
.. [2] ``sudo service apparmor teardown`` and ``sudo update-rc.d -f apparmor remove``
.. [3] ``export no_proxy="127.0.0.1,[::1],localhost,192.168.124.0/24,172.16.0.0/12"``
.. [4] ``ssh-keygen -t rsa``
.. [5] ``sudo sed -ie "s/%sudo\tALL=(ALL:ALL) ALL/%sudo ALL=(ALL) NOPASSWD:ALL/g" /etc/sudoers``
.. [6] ``tools/docker-admin --daemon centos ./production.sh admin.rebar.digital``
.. [7] ``sudo ip a add 192.168.124.4/24 dev docker0``
.. [8] ``curl -sSL https://get.docker.com/ -o /tmp/docker.sh || sh``
.. [9] ``docker-compose up | grep -v logstash``
.. [10] ``docker-compose stop``
.. [11] ``docker-compose rm``
