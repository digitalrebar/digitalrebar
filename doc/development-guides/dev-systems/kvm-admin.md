## Admin Node in KVM

It is possible (and convienent) to run a OpenCrowbar admin node in a
KVM container.  To do so, you need to be running
in a development environment that can run KVM, Ruby and other tools.  

> Once the admin node is running in a container, it will keep running until you exit the script

### Using the ocb-br bridge

...

### Advance Options

Allows admins to quickly go back to a current check point

To use it:

1: Create an admin disk by running `kvm-admin --create-image [filename]`
1: Setup the admin node as normal.
1: Once the admin node has converged:
  1: `rm /etc/udev/rules.d/persist`
  1: `halt`
1: After the system has halted, kill the kvm-admin script. This
will cause the disk the admin node was using to be converted into
a compressed qcow2 image stored in 
1: When you are ready to run the demo, run `kvm-admin --demo [filename]`

### Ensuring that the admin node can deploy operating systems to slaves

KVM admin copies in an initial ISO, but you can add them to to the cache...

### Running a production mode OpenCrowbar admin node 

To do that, run the following command from the core repository:

  1. `cd /opt/opencrowbar/core1
  1. `./production.sh admin.smoke.test`

> note: the crowbar-bootstrap step takes a while, be patient


You should be able to monitor the progress of the admin node
deployment at http://192.168.124.10:3000.  Once the admin node is finished
deploying (or if anything goes wrong), you will be left at a running
shell inside the container.

You can ssh into the VM from the host by finding its IP address.  
The script will preload your keys into VM.

### Booting slave VMs from the admin node

Prereq : `sudo apt-get install bridge-utils`

Use the `kvm-slave` command

#### Bare Metal (the easy way)
If your development environment is running on bare metal (as opposed
to running inside a VM), you can use `tools/kvm-slave &` to spawn a
KVM virtual machine that will boot from the freshly-deployed
admin node.

#### Real Hardware slaves

To boot Real Hardware, bind a physical interface to ocb-br with brctl, make sure that interface is up and does not have an address, and plug it in to a switch that has the physical boxes you want to boot.

Example Commands:
  1. slave the eth2 to the ocb-br bridge, `sudo brctl addif ocb-br eth2`
  1. turn on eth2 for the bridge, `sudo ip link set eth2 up`
  1. boot the physical nodes from a switch connected to eth2
