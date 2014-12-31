# OpenCrowbar Directory Structure
A complete OpenCrowbar installation consists of a **core** component, and workloads (such as **openstack**, **hadoop**, **hardware**).  The base installation directory in a production environment will consist of:

## OpenCrowbar Top-level directory layout
| **Directory** | **Description** | **Role** |
|----------------------------|:-----------------------------------------------------------|:--------------|
| /opt/opencrowbar/core | **Core** OpenCrowbar Complete system foundation and WebUI | __Mandatory__ |
| /opt/opencrowbar/hadoop | **Hadoop** workload components | __Optional__  |
| /opt/opencrowbar/hardware | **RAID and BIOS Hardware Provisioner** workload components | __Optional__  |
| /opt/opencrowbar/openstack | **OpenStack** workload components | __Optional__  |
| /opt/opencrowbar/ceph | **Ceph** worload components | __Optional__  |

The above components each have their own GIT repository. The OpenCrowbar **core** repository is the essential, engineer that drives OpenCrowbar and can operate alone from any of the workload components.  The workload components of OpenCrowbar have built-in dependencies on the **core**.

## OpenCrowbar core directory layout
The directory structure of the **core** consists of these parts under the _/opt/opencrowbar/core_ directory:

| Sub-Directory | Description of Contents |
|-------------------|:--------------------------------------------------------------|
| BDD | Business Driven Development Testing Infrastructre |
| barclamps | Metadata that for barclamps that drive workload deployment |
| bin | OpenCrowbar executables and helper files |
| bootstrap | Utility files used to ready the **core** for operation |
| clients | API clients applications |
| doc | The OpenCrowbar **core** documentation suite |
| etc | Target deployment platform OpenCrowbar stop/start scripts |
| jig - chef | The **core** cookbook recipes for OpenCrowbar node-role drivers |
| jig - noop | Support infrastructure for the __no-op__ jig |
| jig - script | Support for the __script__ jig |
| rails | core only ruby-on-rails infrastructure support tools and utilities |
| rails-engines | ruby-on-rails support for workload deployment |
| setup | Tools and utilities to help get OpenCrowbar bootstrapped |
| sledgehammer | Contains the utilities used to generate the PXE boot image |
| smoketest | Automated self-test validation drivers |
| test | Additional role test facilities |
| tools | Tools used to allow enable the *admin* node to manage OS installed slaves |
| updates | Merge tools |

Role configuration information is located within the jig infrastructure.
