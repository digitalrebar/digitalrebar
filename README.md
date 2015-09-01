[![Stories in Ready](https://badge.waffle.io/opencrowbar/core.png?label=ready&title=Ready)](https://waffle.io/opencrowbar/core)
# Welcome to OpenCrowbar
[![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/digitalrebar/core?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Welcome to the OpenCrowbar Project - the gateway to a new hardware provisioning experience that delivers the best of software deployment automation and orchestration. OpenCrowbar is a successor of the 4 year-old & still active [Crowbar project](http://github.com/crowbar). It derives much of its functionality from its predecessor, but offers a lot more.

## Getting Started & Documentation

Documentation close by and located in under the **/doc** directory of Digital Rebar and for each workload module.

Want to _try out OpenCrowbar_?  Install following the [**deployment guide (for Centos)**](/doc/deployment-guide).
For the quick start, just go [here](/doc/deployment-guide/Install-CentOS-RHEL-6.6-AdminNode.md) to build an admin node.

Please refer to the [/doc directories](/doc/README.md) for detailed information.  We attempt to define and maintain one sub-directory for each functional element.  This structure is intended to be common across all workloads in the [OpenCrowbar project](https://github.com/opencrowbar/)

> Please, do NOT add documentation in locations outside of the  **/doc** directory trees!  If necessary, expand this README to include pointers to important **/doc** information.

We've also made it easy to setup and run latest Crowbar using our [**Fast-Start (with Docker)**](/doc/development-guides/dev-systems) as a rapid-fire code/test/commit cycle tool.  

## Current Release

* Release v2.3 as of 7/30/2015 (was known as _Drill_) on *master* branch
* Development for _Epoxy_ cycle moved to *develop* branch
* Earlier Releases: 
  * Anvil became v2.0 in Apr 2014
  * Broom became v2.1 in Dec 2014
  * Camshaft became v2.2 in Apr 2015

## Join Our Community

* [Live Weekly Planning and Design Meetings](http://bit.ly/crowbar-calendar)
* [Mailing List](http://bit.ly/crowbarlist)
* [![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/digitalrebar/core?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
* #Crowbar IRC channel on Freenode  - kinda quiet - best to find us on list.

## Exec Summary 

The principal motivation for creation of OpenCrowbar is the transition a from bare metal installer into a tool that manages ongoing operations.  OpenCrowbar enables upgrade and continuous deployment automation. This capability is important for large scale deployments of evolving complex projects like OpenStack, Hadoop, and Ceph.

OpenCrowbar provides the foundation for operations automation. OpenCrowbar is an open reference implementation that can be reliably deployed in large-scale, multi-site datacenters.  This effectively productizes best practices in a way that allows creation of consistent and discoverable operating environments.  Users benefit with fast time-to-value and we benefit by having consistent installations across the ecosystem.  

## Benefits of OpenCrowbar

OpenCrowbar reduces the cost of datacenter hardware infrastructure preparation. The immediate benefits of OpenCrowbar are realized in the deployment of complex compute and storage clusters. OpenCrowbar reduces the human resource cost of getting compute platform systems into productive use, automates continuous hardware redeployment, automates hypervisor, operating systems, and application layer software installation and management. All of these add up to significant gains in operational reliability, consistency, and concomitant reduction in defects handling costs.
