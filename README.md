# Welcome to [Digital Rebar](http://rebar.digital)

The [Digital Rebar](http://rebar.digital) is a container-ready cloud & hardware provisioning platform that delivers the best of software deployment automation and orchestration without locking you into a single platform or operating system.  Our mission is to embrace the heterogenous nature of cloud and data center operations.

While it's been completely rebuilt by DevOps artisans, Digital Rebar history includes years of battle-tested ops learnings by the [Crowbar Project](http://github.com/crowbar) founders.

## Getting Started & Documentation

Documentation is maintained in [Read The Docs](http://digital-rebar.readthedocs.io/en/latest/) and sourced from our [**doc** repository](https://github.com/digitalrebar/doc).

## Help & Community

* Help & Chat
  * Gitter: [![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/digitalrebar/core?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
* [Documentation](http://digital-rebar.readthedocs.io/en/latest/) 
* [Live Weekly Planning and Design Meetings](http://bit.ly/digitalrebarcalendar)
* [Mailing List](http://bit.ly/digitalrebarlist)

Commercial support for Digital Rebar is available from [RackN Inc](http://rackn.com).

## Codebase History

* Fall 2016: Digital Rebar / DigitalRebar converged into single repo + workloads.
* Summer 2015: Digital Rebar / Core (and many repos) restructured into microservice containers
* Fall 2013: OpenCrowbar Rearchitecture (v2) was a complete re-write of Crowbar to be composable
* Spring 2011: Original Project: Crowbar (still maintained by SUSE) was a Chef wrapper layer

## Dangerous and Fun Quick Start command.

_We know you'll ignore this advice, but we recommend that you [Read The Docs](http://digital-rebar.readthedocs.io/en/latest/) Deployment Install Guide first!_

WARNING: This is only for clean systems.  If you've already cloned this repo, use the ``run-in-system`` script!

From the home directory of a user with sudo capabilities,

``curl -fsSL https://raw.githubusercontent.com/digitalrebar/digitalrebar/master/deploy/quickstart.sh | bash``

This command will turn the current node into a DigitalRebar admin node in Host mode operating as the IP on 
the default gateway interface.  For cloud instances, this means that it will use the private network and will
only safely manage nodes in its private network.  UX and API will be available through the public IP of the 
cloud instance assuming https is allowed through the cloud's network protections. 

You may add additional arguments to bash to enable features or change the IP address that ADMIN node will use.

E.g.  ``curl -fsSL https://raw.githubusercontent.com/digitalrebar/digitalrebar/master/deploy/quickstart.sh | bash -s -- --con-provisioner --con-dhcp --admin-ip=1.1.2.3/24``

will enable the dhcp server and provisioner for the admin node.  You will need to edit the admin-internal network to
boot nodes properly.  This would also set the admin-ip to 1.1.2.3/24 in the configuration files.  This last part
is needed if you are using an AWS or google instance and you want to use your admin node for things not directly
in your VPC/Network.

NOTE: When enabling the provisioner, you will need about 20GB of disk space.  Plan accordingly.