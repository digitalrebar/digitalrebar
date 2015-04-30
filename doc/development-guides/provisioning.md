## Provising Process

**Overview**

The Provisioner provides the roles and recipes to set up the provisioning server and a base environment for all 
provisioned nodes. The Provisioner also provides the transition entry point for nodes that need to have DHCP transitions
done. The Provisioner assumes that addressing will be handled outside of this barclamp.

**Roles**

The following node roles are defined:

   * Provisioner-server 
      * Configures the system to run the provisioning services (TFTP, Web server with apt packages, APT repository)
   * Provisioner-service
      * Allows for the injection of the provisioner information into other components
   * DHCP-server
      * Provides a DHCP server for doing initial device discovery
      * Points to the provisioner-service
   * DHCP-database
      * Updates the DHCP information based upon the bootenv properties.
   * Provisioner-database
      * Updates the provisioner boot information based upon the bootenv properties
   * Provisioner-repos 
      * Makes sure that the apt/yum repository is configured and a root ssh key is deployed
   * Provisioner-setup-base 
      * Prepares boot environments from ISOs


**Workflow**

The provisioner provides the Sledgehammer discovery image.  It is expected that there is a DHCP server,
OpenCrowbar provided or external to OpenCrowbar, that will allocate an address and point to the provisioner.

The Sledgehammer discovery image will register the node in OpenCrowbar and allocate an admin address.  Once the node
is created, the database roles will update the provisioner and dhcp server to enable future control for operating system
installation, local booting, or other boot environments as needed.

New operating systems can be seen [Adding Operating Systems](../deployment-guide/adding-operating-systems.md).

After an operating system is installed, the system injects code to update OpenCrowbar that the node has completed
installation.  The database roles will update the provisioner and dhcp servers to boot from local drives.

