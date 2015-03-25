## External Services Configuration

The OpenCrowbar System can be configured to utilize external (not installed or managed by OpenCrowbar) services.  These configurations are made in the following file /opt/opencrowbar/core/crowbar-config.sh and must be made *PROR* to running production.sh to install the system.  Users can configure any combination of of these services.

### DNS (Domain Name Server)

By default OpenCrowbar will utilize Bind (named) on the Admin Node when you install OpenCrowbar.  All clients will be configured to use this system when they are installed.  DNS entries are automatically updated as new nodes are configured or removed from OpenCrowbar.  OpenCrowbar can be configured to use an already existing DNS however it becomes the responsability of the user to ensure that entries are created or removed as OpenCrowbar will not function correctly if the name and IP Address are not configured with the ones assigned by OpenCrowbar (Typically MAC address of the primary interface)

*  Edit /opt/opencrowbar/core/crowbar-config.sh
*  Find the line 'crowbar roles bind dns-database to "$FQDN"' and comment it out by adding # to the front of the line.
*  Find the line '#curl -X PUT -d '{"Datacenter": "dc1", "Node": "external", "Address": "192.168.124.11", "Service": {"Service": "dns-service", "Port": 43, "Tags": [ "system" ]} }' http://127.0.0.1:8500/v1/catalog/register' uncomment it by removing the # from the front of the line and then change the IP address of the DNS server (192.168.124.11 in this example) to the IP of the system which will be serving DNS for the enviornment
*  Save the file and continue on with the remainder of the installation steps.

###  NTP (Network Time Protocol)

By default OpenCrowbar will utilize NTP (NTPd) on the Admin node and all clients will be configured to use this server to sync their time.  OpenCrowbar can be configured to not run NTP on the admin server and configure any nodes installed to point their clients to the IP address specified in this file.

*  Edit /opt/opencrowbar/core/crowbar-config.sh
*  Find the line 'crowbar roles bind ntp-server to "$FQDN" and comment it out by adding # to the front of the line
*   Find the line '#curl -X PUT -d '{"Datacenter": "dc1", "Node": "external", "Address": "192.168.124.11", "Service": {"Service": "ntp-service", "Port": 123, "Tags": [ "system" ]} }'       http://127.0.0.1:8500/v1/catalog/register' uncomment it by removing the # from the front of the line and then change the IP address of the NTP server (192.168.124.11 in this example) to the IP of the system which will be serving DNS for the enviornment.
*  Save the file and continue on with the remainder of the installation steps.

###  DHCP (Dynamic Host Configuration Protocol)

By default OpenCrowbar will configure DHCP (dhcpd) on the Admin node and utilize it to preform boot sequences to nodes as they transition from discovery to OS installation.  Additionally it will direct a system to use it's local drives to boot once an OS is installed.  OpenCrowbar can be configured to use an already existing DHCP Server however it becomes the responsability of the user to ensure that nodes keep the same IP address.  

*  Edit /opt/opencrowbar/core/crowbar-config.sh
*  Find the line 'crowbar roles bind dhcp-database to "$FQDN"' and comment it out by adding # to the front of the line.
*  Save the file and continue on with the remainder of the installation steps.
*  On the DHCP server the user must redirect the node which is booting to the OpenCrowbar Admin server.  To do this add the following stanza to /etc/dhcpd/dhcpd.conf

```
subnet 192.168.124.0 netmask 255.255.255.0 {
 option routers 192.168.124.10;
 option subnet-mask 255.255.255.0;
 option broadcast-address 192.168.124.255;
 option domain-name "neode.com";
 option domain-name-servers 192.168.124.11;
 default-lease-time 7200;
 max-lease-time 36000;
  pool {
    range 192.168.124.81 192.168.124.254;
    allow unknown-clients;
    if option arch = 00:06 {
     filename = "discovery/bootia32.efi";
  } else if option arch = 00:07 {
     filename = "discovery/bootx64.efi";
  } else {
     filename = "discovery/pxelinux.0";
  }
    next-server 192.168.124.10;
  }   
}
```
*  Replace the subet, domain-name and pool information to match the enviornment.
*  Put the IP address of the OpenCrowbar Admin Node in the next-server in place of 192.168.124.10.
