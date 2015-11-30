# Digital Rebar Deploy Scripts

> Maintained by RackN, Inc.

Items in this repo are used to install and configure
[Digital Rebar](https://digitalrebar.githib.io) and associated workloads.

## Documentation

The installation process is documented in the main
[Digital Rebar documentation](https://github.com/digitalrebar/doc) and the
[install page](https://github.com/digitalrebar/doc/deployment/install.rst).

## This Just Might Work Quick Start Helps

Here are some stuffs that just might work for a quick start on Mac OSX or Linux.
See the above for all the gory details.

### Steps

```shell
mkdir digitalrebar
cd digitalrebar
git clone https://github.com/rackn/digitalrebar-deploy deploy
cd deploy/compose
ln -s ../../../digitalrebar digitalrebar 
cd ..
```

For Mac OSX, run (and fix missing items):
```shell
./run-in-mac.sh
```

At the end of this process, you will have an admin node running in a virtual box VM as a set of docker containers.  The access mode will be HOST mode and the access address will default to 192.168.99.100/24.  This assumes that your defaults matched our defaults.

At this point, you should be able to create PXE booting VMs or use vagrant boxes with a join script to continue playing with the system.

All Mac OS X questions can be answered [here](https://github.com/digitalrebar/doc/deployment/install/mac.rst).

For Linux OSes (RedHat-based or Debian-Based), run (and fix missing items):
```shell
./run-in-system.sh --localhost
```

At the end of this process, you will have an admin node running in containers on the local system.  The access mode will be FORWARDER mode and the default address will be 192.168.124.11/24.  Forwarder mode allows a little easier control of KVM-based systems.

At this point, you should be able to create PXE booting VMs by using the tools/kvm-slave in the core tree with something like:
```shell
cd digitalrebar/core
tools/kvm-slave
```

All local Linux-based questions can be answered [here](https://github.com/digitalrebar/doc/deployment/install/local_linux.rst).

At this point for either method, you have a tree that can be used for development or just running DigitalRebar.

### Reset Steps

It is sometimes useful to restart or reset the environment.  This can be done as follows.

To stop/clean-up the system, do the following:
```shell
cd digitalrebar/deploy/compose
docker-compose kill
docker-compose rm -f
```

To start the system again, do the following:
```shell
cd digitalrebar/deploy/compose
docker-compose up -d
```

Or to do it development style, described better [here](https://github.com/digitalrebar/doc/deployment/install/dev_mode.rst):
```shell
cd digitalrebar
# Edit code to heart's content in subtrees
cd digitablrebar/core
tools/docker-admin
```

This leaves you in a shell that you can run docker-compose commands from.  Exiting the shell will quickly clean up the environment again.

