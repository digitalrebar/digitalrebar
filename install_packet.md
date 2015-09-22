# Running on [Packet.net](http://packet.net)

The following instructions require that you have an account at [Packet.net](https://app.packet.net/#/registration).  They could be adapted to work for other hosting providers since the Ansible script used is generic.

1. Create an account at Packet.net
  1. Note your API key
  1. Register your SSH public key
1. Create a `~/.dr_info` file
  1. add the line `API_KEY=[your key]`
1. `git clone https://github.com/rackn/digitalrebar-deploy.git packet`
1. `cd packet`
1. `./run_in_packet.sh yourname`

Watch the results - when the Ansible run completes you can visit [ip]:8500 and [ip]:3000.

To start slaves:

1. login to the machine as root using `ssh -X root@[ip]`  (the -X allows you to see the VM boot screens using X)
1. `cd compose/digitalrebar/core`
1. `tools/kvm-slave &` for each system you want to bring up.  The Packet systems can easily handle 3 to 5.

Remember to DESTROY the system when you are done.  The meter is running at $0.40/hour!
