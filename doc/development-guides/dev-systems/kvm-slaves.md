## Test/Dev using KVM Worker Nodes

Crowbar developers are _strongly_ encouraged to always build and test deployment code in multi-node situations.

### Using kvm-slaves with _kvm-slaves_ script

> closing the KVM window will *not* stop the VM because the scripts are designed to restart the VM if it halts.

#### Prereqs

There are some, run the script and it will tell you!  Some of the requirements are ruby gems.

#### Start Up

From the dev system, `tools/kvm-slave`

This creates a KVM machine and attaches it to the Docker bridge.

### Shutdown 

If you kill the pid of the kvm-slave, it will exit gracefully.

### Common Usage

Most of our kvm-slave usage looks like:

* Create 3: `for j in 1 2 3; do tools/kvm-slave & done`
* Destroy 3: `for j in 1 2 3; do kill %$j ; done`