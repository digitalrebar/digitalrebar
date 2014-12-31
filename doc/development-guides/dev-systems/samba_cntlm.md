# Windows & Firewall Help

These are optional Items that we find handy if you are developing on Windows using VMs and/or behind corporate firewalls
    
## SAMBA share (allows dev editing from Windows hosts)

  1. ubuntu: `sudo apt-get install samba`
  1. `sudo vi /etc/samba/smb.conf` 
    1. at the bottom add [the example of share block](https://gist.github.com/cloudedge/298121043ea8ec2b9620) to add 
    1. edit to match your system if needed
    1. `sudo service smbd restart`
  1. connect from your host using `\\[machine address]\crowbar_dev`

## CNTLM proxy (allows storing user names for authenticated proxies)

VMs and Containers are not "local" and require your CNTLM proxy to act as a gateway

  1. ubuntu: `sudo apt-get install cntlm`
  1. `sudo vi `/etc/cntlm.conf`
    1. make sure that you allow NON local hosts to use the proxy (set `gateway yes`)!
    1. add your credentials
    1. make sure the port is 3128
  1. `sudo service cntlm restart`
  
You likely also need to tell your Squid proxy to use CNTLM! With the following additional Squid config lines (assumed CNTLM using port 3128):

  always_direct allow to_localnet
  always_direct allow to_localhost
  cache_peer 127.0.0.1 parent 3128 0 default
  never_direct allow all
