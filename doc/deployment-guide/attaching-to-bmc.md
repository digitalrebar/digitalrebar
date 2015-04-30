
## Connecting to the BMC Network

By default, Crowbar sets up a BMC network on 192.168.128.xxx/24 named `the_bmc` in the category `bmc` and the group `internal`.  
You may, of course, modify this by changing values from the `/networks` page.  These instructions have been created with the 
assumption that you are using the default network and should be modified to match your specific configuration.

### Attaching Admin to BMC

While Crowbar will configure the admin node and managed node BMCs, it does not configure a gateway for your workstation 
to connect to the nodes on that network.  You need to add a gateway IP on the BMC network from the system you are using
to connect to the BMC network.

These instructions assume you are using a Linux desktop with the Admin node running in a docker container.  The container
is using docker0 as the network bridge to the nodes.

You must add the bmc range the bridge from your workstation: `sudo ip addr add 192.168.128.1/24 dev docker0`
  
You should now be able to ping the node's BMC interfaces.  By default they are assigned on from 192.168.128.21, so 
`ping 192.168.128.21` should work.

The IPs for the BMC network should be visible on the `/network_map` page in the UI and node detail page.

### Remote Manage Web UI

Once you know the node's BMC IP address and have network access to that network, you should be able to open the node's 
Web Management interface (if your node has one).

From a browser: `https://[node ip]` should bring up a login prompt.

The default login is `root`/`cr0wBar!`
