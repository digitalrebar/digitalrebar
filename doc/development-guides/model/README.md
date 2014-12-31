# Core OCB Objects Design Information#

There are 3 basic objects that everything else in OpenCrowbar relies
on -- node objects, roles, and noderole objects.

* **Node objects** encapsulate machine-specific state -- they have unique
programmatically generated names (which must also be the machine's FQDN
in DNS), track whether OpenCrowbar is allowed to manage the machine, and track
whether the machine is alive and reachable. In the OCB framework,
nodes are things that a jig performs operations on as directed by a
role through a noderole.

* **Roles** are the primary unit of functionality in OCB -- they provide
the code that the jigs use to effect change on the nodes in accordance
with the desired state stored in the noderole graph. Roles form a
dependency graph of their own (roles must declare what other roles
they depend on), each role must declare the jig that will be used to
do things to a node, and roles can have flags that affect how the
annealer will handle certian aspects of building the noderole graph
and initial node bootstrap.

* **Noderoles** represent a binding of a role to a node.  Each noderole
tracks any state that needs to communicated from the user or the
OpenCrowbar framework to a node (and vice versa), and the overall noderole
graph determines both the order in which roles are enacted on nodes
and what attributes are visible from other noderoles when the noderole
runs.

On top of those 3 basic object types, we have 2 more that are used to
help keep cluster administrators from dying of information overload
when staring at a noderole graph with 10,000 edges.  These are
deployments and deployment roles.

* A **deployment** is an administratively convenient logical grouping of
nodes along with a set of default role configurations (the deployment
roles) relevant to whatever workload is being run in the
deployment. Deployments all have a parent deployment except for the
system deployment, which OpenCrowbar manages and which is where all
newly-discovered nodes wind up. Nodes belong to deployments, which
helps control how the noderole graph is built.  Deployments can be
either proposed (when the user needs to make deployment-wide
configuration changes), or committed (where the annealer is allowed to
work, and user-level changes are Not Allowed)

Additionally, we have barclamps to group together logically related
roles, glue the roles into the OpenCrowbar API and Web UI, and contain any
jig-specific files that the roles require.

## Roles, in a little detail: ##

Roles have a lot to do.  Their dependency graph is used a template to
build the noderole DAG, they need to provide their jig with all the
code and data it will need to effect the changes that the role wants
on a node, they need to ensure that the noderole graph is build
properly, and in some cases they need to track state that should not
be represented directly in the noderole graph.

Most roles should not need to have any state outside of the state stored in the
noderole graph, but there are some (primarily those provided by the
network, dns, and provisioner barclamps) that need to maintain a
significant amout of state outside the noderole graph or that need to
be able to react to noderole and node state transitions. To give them
a formal method of doing so, you can override the base Role model with
one that responds to several events that happen in the noderole and
node lifecycles.

### The Two Rules for Events: ###
1. Events run synchronously, so they must be fast.

    If your event takes more than a few milliseconds to run, or you
    want to do something on a remote machine, you should make it a
    role of its own and bind it to that node as a noderole instead.

 2: Events must be idempotent.
     If the work you were going to do has already been done, don't do it again.

### How To Respond to Events: ###

The base Role model has a mechanism for letting Rails dynamically
subclass it if there is an appropriatly named model in the Rails
engine that the barclamp provides -- you can provide an override based
on the name of the role by providing

    class BarclampFoo::RoleName < Role

class, and you can provide a general Role override for your barclamp
by providing

    class BarclampFoo::Role < Role

### Noderole Events: ###

You can provide event hooks on your roles that work with noderoles at
6 points in their lifecycle:

* on\_proposed
* on\_todo
* on\_blocked
* on\_transition
* on\_error
* on\_active

Each method will be called with the noderole that just completed its
state transition after the noderole has transitioned to the state, and
all of its child noderoles have had their state updated accordingly.

### Node Events: ###

You can provide event hooks on your roles that work with nodes at 2
points in their lifecycle for now:

* on\_node\_create will be called after the node is created and the
default set of noderoles has been bound to it.
* on\_node\_delete will be called just before the node is destroyed.

Additionally, there is an on\_node\_change event that gets called just
after Rails saves any changes to a node object.

### Role Flags: ###

Right now, roles have 4 flags that the OpenCrowbar framework knows how to
handle:

1. Discovery, which means that this role will be automatically bound
to all non-admin nodes when the node is freshly-created if the role's
jig is active.
2. Bootstrap, which means that this role will be automatically bound
to all freshly-created admin nodes.  This flag is primarily used by
the OpenCrowbar framework to bootstrap the initial OpenCrowbar admin node into
existence.
3. Implicit, which signals that this role can be implicitly created
and bound to a node as part of the dependency resolution process, and
that it must be bound to the same node as the role that depends on it
is being bound to.
4. Library, which is not used by anything right now and may be
removed.

### Role dependency rules: ###

Each role must declare what other roles it directly depends on, and
those dependencies are not allowed to be cyclic -- a role cannot
directly or indirectly depend on itself.  Roles should not declare a
dependency on a role it only indirectly depends on, as that makes the
dependency graph needlessly more complicated.  A role is dependent on
another role if that other role must be deployed somewhere in the
cluster before the current role.

## Noderoles: ##

### How the noderole graph is built: ###

Right now, all nodes are ultimately added to the noderole graph via
the add\_to\_node\_in\_deployment function on role objects.  You pass it a
node and a deployment, and it either creates a node role bound to an
appropriate place in the graph or dies with an exception.  In detail:

1. Verify that the jig that implements the role is active.
2. Check to see if this role has already been bound to this node. If
it has, return that noderole.
3. Check that all our parent roles have been bound into the noderole
   graph.  If they have not, bind them on the same node we are binding
   to.
4. Create a new noderole binding this role to the requested node in
the deployment, and create parent/child relationships between the new
noderole and the parents we found.  The noderole will be created in
the PROPOSED state.
5. Call the on_proposed event hook for this role with the new
noderole.
6. Return the new noderole to the caller.

This function will need to grow more ornate when we want to start
supporting more than just the system deployment -- right now it will
not respect deployment-level scoping.  Adding it is a fairly
straightforward extension to the tests in step 4.  This function is
also arguably one of the more critical pieces of code in the OpenCrowbar
framework -- it determines the shape and connectedness of the noderole
graph, and hence it plays a large part in determining whether what we
are deploying makes sense.

### What is in a noderole: ###

1. Pointers to its parents and children in the noderole graph.
2. The state of the noderole.
3. A blob of JSON that the user can edit.  This blob is seeded from
the deployment role data, which in turn is seeded from the role
template
4. A blob of data that the OpenCrowbar framework can edit.  This is used
by the roles to pass system-generated data to the jigs, and is usually
seeded by one of the noderole events.
5. A blob of data that we get back at the end of a jig run.

## What happens in OCB to create a node: ##

1. an API request come in with the requested name of the new node, and
a flag that indicates whether it is an admin node.
2. The requested name is checked to see it is a valid FQDN in the
cluster's administrative DNS domain and that it is unique.  If neither
of those are true, the request fails, otherwise we create the node
object.  The new node object will not be alive or available, and it
will not have any roles bound to it.
3. (optional) API calls come in to hint to the system (via the
   hint-admin-mac and hint-admin-v4addr attribs) what MAC address
   should be used for DHCP purposes and what IP address should be
   assigned to the node from the admin network.  Nodes booting via
   Sledgehammer use hint-admin-mac to ensure that the
   provisioner-dhcp-database role runs, which allows Sledgehammer to
   get a proper in-range DHCP address.
4. API calls come in that bind the crowbar-managed-node role to the
   freshly-created node.  This will have the side effect of pullng in
   all the roles we need to properly discover a node and bind them to
   the node-role graph as well.
5. (optional) API calls come in that modify the default values of the
freshly-bound noderoles.
6. The node is committed via the node API, which automatically
commits all the noderoles bound to the node.
7. The node is marked as alive by the node API. After that, the
annealer takes over to discover the node.

Creating the initial admin node follows the same process, except we
add the crowbar-admin-node role instead of the crowbar-managed-node role.

## The NodeRole state machine, the framework-driven parts: ##

All noderoles start in PROPOSED state, and they stay there are
committed (either individually, as part of a node commit, or as part
of a deployment commit). From PROPOSED, a noderole
can go to TODO (if the noderole has no parents or all its parents are
ACTIVE), or BLOCKED (if it has any non-ACTIVE parents).

From BLOCKED, a noderole can go to TODO when all of its parents are
ACTIVE.

The annealer looks for noderoles in TODO that meet the following
conditions:

1. The jig that is associated with the noderole via the role half of the binding is active,
2. The deployment that the noderole belongs to is COMMITTED,
3. The node that the noderole binds to is alive and available,
4. There is no noderole for that node that is in TRANSITION

It takes all the noderoles that meet those conditions, sets them in
TRANSITION, and kicks off a delayed job that will wind up setting the
noderole either to ACTIVE or ERROR.

When a noderole is set to ACTIVE, it sets all of its children in
BLOCKED state to TODO if the rest of that child's parents are ACTIVE.

When a noderole is set to ERROR, it transitions all of its children to
BLOCKED if they were not already blocked.

### How we determine what information is visible to a node during a jig run: ###

Right now, we use the dumbest method possible that still obeys scoping
rules.  We deep-merge all the JSON blobs from all the noderoles on
this node that are ACTIVE, deep-merge that with all the JSON blobs
from all the noderoles and deployment roles that are parents of mine,
starting from the most distant set to the closest set, and then
deep-merge that with the JSON blobs from the current noderole.  That
gets handed to the jig, which does its jiggy thing with it and
whatever scripts/cookbooks/modules/whatever, and we get a blob of JSON
back.  We deep diff that blob with the blob we sent to the jig, and
that is what winds up on the noderole's wall.

## Aliveness and availability: ##

Nodes in the OpenCrowbar framework have two related flags that control
whether the annealer can operate on them.

Aliveness is under the control of the OpenCrowbar framework and
encapsulates the framework's idea of whether any given node is
manageable or not.  If a node is pingable and can be SSH'ed into as
root without a password using the credentials of the root user on
the admin node, then the node is alive, otherwise it is dead.
Aliveness is tested everytime a jig tries to do something on a node
-- if a node cannot be pinged and SSH'ed into from at least one of
its addresses on the admin network, it will be marked as
dead.  When a node is marked as dead, all of the noderoles on that
node will be set to either blocked or todo (depending on the state of
their parent noderoles), and those changes will ripple down the
noderole dependency graph to any child noderoles.

Nodes will also mark themselves as alive and dead in the course of
their startup and shutdown routines.

Availability is under the control of the OpenCrowbar cluster
administrators, and should be used by them to tell OpenCrowbar that it
should stop managing noderoles on the node.  When a node is not
available, the annealer will not try to perform any jig runs on a
node, but it will leave the state of the noderoles alone.

A node must be both alive and available for the annealer to perform
operations on it.

## Delayed Jobs and Queuing: ##

The OpenCrowbar framework runs all jig actions in the background using
delayed_jobs + a thin queuing layer that ensures that only one task is
running on a node at any given time.  For now, we limit ourselves to
having up to 10 tasks running in the background at any given time,
which should be enough for the immediate future until we come up with
proper tuning guidelines or auto-tuning code for significantly larger
clusters.

### Postgresql 9.3: ###

Migrating to delayed_jobs for all our background processing made it
immediatly obvious that sqlite is not at all suited to handling real
concurrency once we started doing multiple jig runs on different nodes
at a time. Postgresql is more than capable of handling our forseeable
concurrency and HA use cases, and gives us lots of scope for future
optimizations and scalability.

### Deployment tree: ###

Until now, the only deployment that OpenCrowbar knew about was the
system deployment.  The system deployment, however, cannot be placed
into proposed and therefore cannot be used for anything other than
initial bootstrap and discovery.  To do anything besides
bootstrap the admin node and discover other nodes, we need to create
another deployment to host the additional noderoles needed to allow
other workloads to exist on the cluster.  Right now, you can only
create deployments as shildren of the system deployment, limiting the
deployment tree to being 2 layers deep.

### Provisioner Installing Ubuntu 12.04: ###

Now, we get to the first of tqo big things that were added in the last
week -- the provisioner being able to install Ubuntu 12.04 and bring
the resulting node under management by the rest of the OCB
framework.  This bulds on top of the deployment tree and DHCP/DNS
database role work.  To install Ubuntu 12.04 on a node from the web UI:

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
    provisioner (via the provisioner-dhcp-database on\_node\_change
    hook) to not PXE boot the node anymore.
   * When the nodes reboot off their freshly-installed hard drive, they
    will mark themselves as alive, and the annealer will rerun all of
    the usual discovery roles.

The semi-astute observer will have noticed some obvious bugs and race
conditions in the above sequence of steps.  These have been left in
place in the interest of expediency and as learning oppourtunities for
others who need to get familiar with the OpenCrowbar codebase.

## Bootstrapping OpenCrowbar: ##

Put docs here.
