# Running on Packet.net

The following instructions require that you have an account at Packet.net.  They could be adapted to work for other hosting providers since the Ansible script used is generic.

#. Create an account at Packet.net
  #. Note your API key
  #. Register your SSH public key
#. Create a `~/.dr_info` file
  #. add the line `API_KEY=[your key]`
#. `git clone https://github.com/rackn/digitalrebar-deploy.git packet`
#. `cd packet`
#. `./run_in_packet.sh yourname`

Watch the results - when the Ansible run completes you can visit [ip]:8500 and [ip]:3000.

To start slaves:

#. login to the machine as root using `ssh -X root@[ip]`  (the -X allows you to see the VM boot screens using X)
#. `cd compose/digitalrebar/core`
#. `tools/kvm-slave &` for each system you want to bring up.  The Packet systems can easily handle 3 to 5.

Remember to DESTROY the system when you are done.  The meter is running at $0.40/hour!
