## Deploy Guide

The files located in this directory provide key information sought by software engineers, systems operators, DevOps staff, and end users.

> Help us build this documentation!  Don't know how to do a Github pull request?  Email the update (markdown prefered) to `opencrowbar@googlegroups.com` and recommend where it should go and we'll make it happen.

### Install OpenCrowbar

Here is a guide to [deploy OpenCrowbar onto a CentOS 6.6 or Red Hat Enterprise Linux 6.6 _admin_ node](./Install-CentOS-RHEL-6.6-AdminNode.md).

### Getting the Admin up

Once you've got a working Crowbar Admin node running, you access the UI via port http://[Admin IP]:3000.  The default user is `crowbar` with password `crowbar`.

> All (and more) of the UI capabilities are also exposed by the Crowbar CLI which is automatically setup on the Admin node.

The `production.sh` script will automatically configure the Admin node to a complete working state.  When the Admin node is ready, you can validate that the Admin node is working correctly in several ways:

* the Deployments...System Overview (`/dashboard/layercake`) should should all green checks
* the Deployments...Annealer (`/annealer`) should show no active work
* the Deployments...Deployments...system (`/deployments/system`) should show all green checks
* the Nodes...Nodes (`/nodes`) show should a green check for the Admin node

> You can check these screens _before_ the annealer has completed bringing up the Admin node and watch Crowbar provision the 1st admin node.

### Crowbar Admin Up!  Add Nodes

Once the Crowbar Admin is running, simply booting VMs or physical nodes onto the management network will engage the discovery process (using DHCP and PXE).  Crowbar does not event hook the PXE service (to maintain separation of duties) so please be patient, the default discovery process requires the system to boot the Sledgehammer image which requires time.

Once the node is booted, Crowbar automaticallly adds the node into the system deployment.  The system deployment is a special purpose deployment used for discovery and base management.  Users have limited options to change it; however, you can easily monitor the discovery process watching the Annealer or system deployment screens.  They will show exactly which steps of the discovery progress are pending, acting and completed.

A node is completely discovered when all the system deployment steps (aka "node roles") are complete with green checks.

It is acceptable to configure nodes (the next steps) even before the discovery is complete.  Crowbar will figure out the correct order of operations and perform actions in sequence.

## Ready State Wizard

The Ready State Wizard performs multiple steps for provisioning discovered nodes from a single screen. This convenience eliminates several steps that are commonly performed together.

From the Ready State Wizard Screen,
  1. name the deployment (network will be named the same as the deployment)
  1. describe the network settings (you can change this later from the Networks page)
  1. check the nodes that you want to provision
  1. select the o/s that you want to be installed
  1. accept the changes (this takes you to the deployment page)
  1. commit the deployment to begin.

## Step by Step Instructions

### Create a Deployment

> These steps duplicate the Ready State wizard operations as individual steps

To do additional operations on a node, it must be part of a deployment.

Create a deployment from the Deployment...Deployments (`/deployments`) page.  Provide the name for your deployment (default is `default`) and click the `add` button.

Once the deployment is created, you can navigate directly to the deployment from the Deployment...Deployments submenu or direct using `/deployments/[my deployment]`.

> Deployments are automatically set to have the system deployment as their parent.  In the future, other parents may be set.

### Adding Nodes to a Deployment

Once you have a deployment, you must attach nodes to it before you can take further action.

You can set a node into a deployment from the Nodes list drill down into each node page (`/nodes/[node name]`) or several at a time from the Nodes...Bulk Edit (`/dashboard/list`).  Both methods provide a list of available deployments.

### Adding Networks to a Deployment

Generally, deployments also configure one or more networks.

To create a network, visit the Network...Networks page (`/networks`) and fill out the table row for your network making sure to select your deployment.  Click `add` to create the network.

Creating a network will also create a matching `network-[name]` role in the system.  These roles allow you to bind the network definition to specific nodes in later steps.

> Once you create a network, you cannot change the assigned network.

### Adding Roles to a Deployment

The new deployment page should now have a list of assigned nodes.  Crowbar uses roles to determine the specific configuration required for a deployment.  You manage the deployment using special purpose milestone roles (aka "noop") that represent target states.   Crowbar tracks the subroles needed to achieve the milestone and automatically adds them.

To add a milestone role, select it from the list at the top of the screen and click `add role`.  For these instructions, use the `O/S Installed` role to install an operating system.  

Once a role is added to a deployment, Crowbar will add it an it's dependencies as columns into the deployment matrix.

Clicking on the role header will allow you to make deployment wide configuration changes if that role has configuration settings.

### Installing an OS on a Node

To install an operating system on a node, you must attch the `O/S Installed` milestone role to the node.  

To attach the O/S role to the node, you much click the green plus button.  This will automatically add both the `O/S Installled` and `Install O/S` role.  

Once the O/S role is attached, the icon changes to a wrench.  Clicking the wrench on the  `Install O/S` role allows you to choose which operating system Crowbar will deploy if you've added multiple boot ISOs.

Once the roles are attached, you start the process by clicking the `commit` button.  

Once the deployment is committed, Crowbar will automatically reboot the node and start the O/S install process.
