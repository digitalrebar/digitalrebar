#!/usr/bin/env ruby
# Copyright 2016, RackN Inc

# endpoint for OpenStack
# {
#   os-username: '',
#   os-password: '',
#   os-project-name: '',
#   os-auth-url: '',
#   os-region-name: ''
# }

require './common'

# create server
def openstack_create(endpoint, name)

openstack server create [-h] [-f {shell,table}] [-c COLUMN]
                               [--variable VARIABLE] [--prefix PREFIX] --image
                               <image> --flavor <flavor>
                               [--security-group <security-group-name>]
                               [--key-name <key-name>]
                               [--property <key=value>]
                               [--file <dest-filename=source-filename>]
                               [--user-data <user-data>]
                               [--availability-zone <zone-name>]
                               [--block-device-mapping <dev-name=mapping>]
                               [--nic <nic-config-string>]
                               [--hint <key=value>]
                               <server-name>

  image = "CentOS-7"
  flavor = "gp1.semisonic"
  keyname = "foo"
  base(endpoint, "server create --key-name #{keyname}--image #{image} --flavor #{favor} #{id} -f shell")
end

# upload key
def openstack_addkey(endpoint, name, key)
  base(endpoint, "keypair create #{name} #{key}")
end

# remove key
def openstack_deletekey(endpoint, name)
  base(endpoint, "keypair delete #{name}")
end

# delete provided server
def openstack_delete(endpoint, id)
  base(endpoint, "server delete \'#{id}\'")
end

# reboot provided server
def openstack_reboot(endpoint, id)
  base(endpoint, "server reboot --hard \'#{id}\'")
end

# get list of servers
def openstack_list(endpoint)
  base(endpoint, "server list -f csv")
end

# get server details
def openstack_get(endpoint, id)
  base(endpoint, "server show \'#{id}\' -f csv")

    os-dcf:diskconfig="AUTO"
    os-ext-az:availability_zone="iad-2"
    os-ext-sts:power_state="1"
    os-ext-sts:task_state="None"
    os-ext-sts:vm_state="active"
    os-srv-usg:launched_at="2016-03-18T04:36:07.000000"
    os-srv-usg:terminated_at="None"
    accessipv4=""
    accessipv6=""
    addresses="public=208.113.135.196, 2607:f298:5:101d:f816:3eff:fe25:d05"
    config_drive="True"
    created="2016-03-18T04:35:57Z"
    flavor="gp1.semisonic (50)"
    hostid="2a74b475939b7e5956a2334e4ce2f5675340e0976778b53742aaec99"
    id="9d5e5a03-5820-48e3-9bdf-f5ac18af4c64"
    image="CentOS-7 (c1e8c5b5-bea6-45e9-8202-b8e769b661a4)"
    key_name="rack1"
    name="delete2"
    os-extended-volumes:volumes_attached="[]"
    progress="0"
    properties=""
    security_groups="[{u'name': u'default'}]"
    status="ACTIVE"
    tenant_id="ac8fccd077d04729b4cb5f85506bb336"
    updated="2016-03-18T04:36:07Z"
    user_id="b55909ea28574f479dc9cf5a9b673697"

end

private

# find a flavor matching the requested type
def find_flavor(endpoint, match)
  favor(endpoint)[0]
end

# find an image matching the requested type
def find_image(endpoint, match)
  images(endpoint)[0]
end

# retrieve the flavors
def flavors(endpoint)
  base(endpoint, "openstack flavors list -f csv")
end

# retrieve the images
def images(endpoint)
  base(endpoint, "openstack images list -f csv")
end

# provide the base CLI interface with correct flags
def base(endpoint, cmd)

  cmd = "openstack --os-username \'#{endpoint['os-username']}\' " \
                  "--os-password \'#{endpoint['os-password']}\' " \
                  "--os-project-name \'#{endpoint['os-project-name']}\' " \
                  "--os-region-name \'#{endpoint['os-region-name']}\' " \
                  "--os-auth-url \'#{endpoint['os-auth-url']}\' " \
                  "#{cmd}"

end