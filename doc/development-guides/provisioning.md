## Provising Process

**Overview**

The Provisioner provides the roles and recipes to set up the provisioning server and a base environment for all provisioned nodes. The Provisioner also provides the transition entry point for nodes that need to have DHCP transitions done. The Provisioner assumes that addressing will be handled outside of this barclamp.

**Roles**

The following node roles are defined:

   * Provisioner-server 
      * Configures the system to run the provisioning services (DHCP, TFTP, Web server with apt packages, APT repository)
      * Depends upon the global utils cookbook that provides line add and delete operations.
   * Provisioner-base 
      * Makes sure that the apt repository is configured and a root ssh key is deployed

**Scripts**

There are 4 cookbooks provided with this barclamp.
| *Cookbook* | *Function* |
| dhcp | Provides the raw recipes and LWP that are needed to manage ISC DHCP server |
| tftpd | Provides the raw recipes that manage the tftpd-hpa server |
| nfs-server | Provides the raw recipes that manage the nfs server and exported mount points |
| ubuntu-install | Provides the raw recipes that manage the DHCP cookbook and the installation image directories. |

The barclamp also provides the standard CLI helper script, =crowbar_provisioner=.

**Parameters**

The following parameters are defined for the barclamp.
| *Name* | *Default* | *Description* |||
| default_user | openstack | User to create for external login |||
| default_password | unset | Clear text password to use for external login |||
| default_password_hash | Hash of openstack | MD5 hash of password to use for external login. <span> <pre>printf 'password' | mkpassed -s -m md5</pre> </span> will generate the hash. |||
| web_port | 8091 | <p>The default web port that the repository web server uses</p> |||
| use_local_security | true | This defaults the security updates path in the install to use the admin node instead of the internet. |||
| dhcp | map | This is a map that contains the DHCP parameters (lease-time and state_machine) |||
| lease-time | 60 | The number of seconds a DHCP lease is valid for the system |||
| state_machine | map | This is the state machine that DHCP server will use in this instance of the barclamp |||

While neither is required, one of =default_password= or =default_password_hash= is required.

The DHCP state machine maps transition states to DHCP states. As the transition function is called with a particular state, the map is consulted to determine if a new DHCP state needs to be set for the node. The provisioner, upon detecting a change, will modify and restart the DHCP server.

| *Transition State* | *DHCP State* | *Expected Result of DHCP State* |
| discovered | hwinstall | The hardware install/update image will be loaded on next reboot. The final act of this image will be to set state to =hardware-installed=. |
| hardware-installed | nova_install | The node will be installed with the base image on the next reboot. The final act of the installation process will be to set the state to =installed=. |
| hardware-updated | execute | The execute state is that final DHCP state. This is the localboot state that tells the node to boot from the hard drive. |
| installed | execute | The execute state is that final DHCP state. This is the localboot state that tells the node to boot from the hard drive. |
| update | update | The hardware install/update image will be loaded on the next reboot. The final act of this image will be to set the state to =execute=. |
| ready | execute | The execute state is that final DHCP state. This is the localboot state that tells the node to boot from the hard drive. |
| reset | reset | The reset DHCP state is a special state that means remove all information about this node from the DHCP system. This allows it to enter the system fresh. |
| reinstall | nova_install | The node will be installed with the base image on the next reboot. The final act of the installation process will be to set the state to =installed=. |

The DHCP states are actual DHCP groups created during the ubuntu-install recipe that is called from the =provisioner-server= role. Other OS or states can could removed or added by modifying or adding state table entries.

The default initial operation is for the DHCP server to direct the node to the discovery image that will at completion set the state to =discovered=.

**Operations**

The creation and commit phase of the barclamp don't do anything.

As nodes transition, the first node seen is made the provisioner server. This usually the admin node. All nodes after assumed to be provisioner-base nodes. These are assigned at the =installed= transition. The node transition also handle DHCP state changes for all states. The Provisioner assumes that it will be called for all state transitions.

**Node OS install process**

The Ubuntu-install cookbook provided by this barclamp is where most of the magic happens. The files used for orchastrating the install process are in:

   * in the source tree: \crowbar\change-image\dell\barclamps\provisioner\chef\cookbooks\ubuntu-install
   * on the admin node: /tftpboot/ubuntu_dvd/&lt;image-name&gt;. Currently only the nova_install image is used.

The files involved:

   * net_seed - provides the seed file used by the installer. Key options modified from defaults: apt-get repository location, pre and post install scripts
   * net-pre-install.sh - run by the ubuntu installer as soon as poossible. its only function is to notify crowbar of the state of the node (via a transition function)
   * net-post-install.sh - run by the installer when installation is complete. It notifies crowbar of the state transition, and kickstarts chef-client to get the node to its baseline

The above files are processed by chef on the server by the ubuntu-install cookbook, to tailor them to the environment.

**Limitations**
