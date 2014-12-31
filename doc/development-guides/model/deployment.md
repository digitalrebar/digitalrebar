## Deployment Model

Deployments are the primary scope boundary for work done by OpenCrowbar.

### System Deployment

The System deployment is used for node discovery.

Cannot be placed into proposed and therefore cannot be used for anything other than
initial bootstrap and discovery.  To do anything besides
bootstrap the admin node and discover other nodes, we need to create
another deployment to host the additional noderoles needed to allow
other workloads to exist on the cluster.  Right now, you can only
create deployments as shildren of the system deployment, limiting the
deployment tree to being 2 layers deep.

### Workload Deployments

  1. Create a new deployment, and add the provisioner-os-install role to
that deployment.  In the future you will be able to edit the
deployment role information to change what the default OS for a
deployment should be.

  2. Drag one of the non-admin nodes onto the provisioner-os-install
role.  This will create a proposed noderole binding the
provisioner-os-install role to that node, and in the future you would
be able to change what OS would be installed on that node by editing
that noderole before committing the deployment.

  3. Commit the deployment.  This will cause several things to happen:

  * The freshly-bound noderoles will transition to TODO, which will
    trigger an annealer pass on the noderoles.
  * The annealer will grab all the provisioner-os-install roles that
    are in TODO, set them in TRANSITION, and hand them off to
    delayed_jobs via the queuing system.
  * The delayed_jobs handlers will use the script jig to schedule a
    reboot of the nodes for 60 seconds in the future and then return,
    which will transition the noderole to ACTIVE.
  * In the crowbar framework, the provisioner-os-install role has an
    on_active hook which will change the boot environment of the node
    passed to it via the noderole to the appropriate os install state
    for the OS we want to install, and mark the node as not alive so
    that the annealer will ignore the node while it is being
    installed.
  * The provisioner-dhcp-database role has an on_node_change handler
    that watches for changes in the boot environment of a node.  It
    will see the bootenv change, update the provisioner-dhcp-database
    noderoles with the new bootenv for the node, and then enqueue a
    run of all of the provisioner-dhcp-database roles.
  * delayed_jobs will see the enqueued runs, and run them in the order
    they were submitted.  All the runs sholuld happen before the 60
    seconds has elapsed.
  * When the nodes finally reboot, the DHCP databases should have been
    updated and the nodes will boot into the Uubntu OS installer,
    install, and then set their bootenv to local, which will tell the
    provisioner (via the provisioner-dhcp-database on_node_change
    hook) to not PXE boot the node anymore.
  * When the nodes reboot off their freshly-installed hard drive, they
    will mark themselves as alive, and the annealer will rerun all of
    the usual discovery roles.
