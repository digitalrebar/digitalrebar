This file documents how to enable basic support for handling package
updates in OpenCrowbar.

####Assumptions:

 * You have installed a OpenCrowbar admin mode by creating a OpenCrowbar ISO
   and installing the admin node from it.
 * The provisioner either has access to the Internet or visibility to
   an HTTP proxy that does. The usual way of doing this is by
   allocating a host IP address from the public network to the admin
   node.  See the OpenCrowbar users guide for more information on how to
   do this.
 * If you are going the HTTP proxy route, that proxy does not require
   any form of authentication.
 * You built your OpenCrowbar ISO from a OpenCrowbar checkout that is recent
   enough to have this file in it.


####Enabling Online Mode

 1: In the OpenCrowbar web UI, click on Barclamps, click on Provisioner, and
    then click on Edit.  This will open the editing pane for the default
    provisioner proposal.
 2: Find the "online": value, and change it from false to true.
 3: If you need to use an upstream HTTP proxy, find the
    "upstream_proxy": value and change it to contain the address and
    port of the proxy in the following format: "address:port"
 4: Click Save, then click Apply.  Once the proposal is finished
    applying, apt/yum/gem on all the nodes will be configured to allow
    package updates from the Internet.

####Caveats:

 * This is still a work in progress.  OpenCrowbar does not have any
 controls around what packages may or may not be updated, so it is
 possible that updating certian packages may break your cluster or
 whatever applications your barclamps are managing.
