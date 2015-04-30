### Advanced Deployments

While docker-based developments or simple developments can function will with the default configuration, many cases will
need customization.  From networking to external services to initial configuration values, the crowbar-config.sh script 
currently controls how the admin components are deployed.

The production.sh script is used to stage, install, and configuration the initial OpenCrowbar admin server.  Customizing
crowbar-config.sh script allows for additional configurations and modifications.

[External Services](./external-services.md) describes how to turn off services in OpenCrowbar and use external replacements.

Another customization allowed is around networking.  The default system configuration assumes that the admin network will
contain the admin server.  This allocation is done by the following line:

```
crowbar roles bind "network-$admin_net_name" to "$FQDN"
```

Commenting this line out will not allocate an admin address for that network.  The admin server providing the provisioner
will need to be routable from the other admin networks to network the admin server is on.  The implication of this is that
the admin node does not have to be on an admin network only routable to admin networks.

The network category system allows for the creation of multiple admin networks.  Creating a new network is accomplished by
adding a new network with a category of *admin*.  The DHCP server, if configured, will update and serve that network as well.
The only assumption is that the new admin network has DHCP relay agent or helper that forwards the DHCP requests to the admin
node or has a DHCP server that forces the nodes to PXE boot to the admin node.

The default OpenCrowbar environment builds an admin network and a BMC network that are paired by a common group field value.
Additional admin networks can be joined to the existing BMC network by using the same group.  If a different BMC network is
needed it can be created and associated with its cooresponding admin network through a common group value.

If the admin node is not part of the admin network, additional networks can be added at any time in the future.