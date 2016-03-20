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

module OpenStack

  # create server
  def self.create(endpoint, name, keyname, image="CentOS", flavor="2048")
    images = images(endpoint)
    image_id = find_image(images, image)
    flavors = flavors(endpoint)
    flavor_id = find_flavor(flavors, flavor)
    keyname = "foo"
    params = "--key-name \'#{keyname}\' " \
             "--image \'#{image_id}\' " \
             "--flavor \'#{flavor_id}\' " \
             "\'#{name}\'"
    raw = base endpoint, "server create #{params}", "-f shell"
    o = unshell(raw)
    id = o["id"]
    return o rescue nil
  end

  # upload key
  def self.addkey(endpoint, name, key)
    log "OpenStack adding key #{key}"
    base endpoint, "keypair create \'#{name}\' \'#{key}\'"
  end

  # remove key
  def self.deletekey(endpoint, name)
    log "OpenStack removed key #{key}"
    base endpoint, "keypair delete \'#{name}\'"
  end

  # delete provided server
  def self.delete(endpoint, id)
    log "OpenStack deleting #{id}"
    base endpoint, "server delete \'#{id}\'"
  end

  # reboot provided server
  def self.reboot(endpoint, id)
    base endpoint, "server reboot --hard \'#{id}\'"
  end

  # get list of servers
  def self.list(endpoint)
    raw = base(endpoint, "server list", "-f csv")
    o = uncsv raw
    log("openstack.list found #{o.count} servers")
    return o
  end

  # find GUID of server from name
  def self.find_id(endpoint, name)
    servers = list endpoint
    servers.each do |k, v|
      return k if v == name
    end
    return nil
  end

  # get server details
  def self.get(endpoint, id)
    raw = base(endpoint, "server show \'#{id}\'", "-f shell")
    o = unshell raw
    log("openstack.get #{o["name"]} is #{o["id"]}")
    return o

    # os-dcf:diskconfig="AUTO"
    # os-ext-az:availability_zone="iad-2"
    # os-ext-sts:power_state="1"
    # os-ext-sts:task_state="None"
    # os-ext-sts:vm_state="active"
    # os-srv-usg:launched_at="2016-03-18T04:36:07.000000"
    # os-srv-usg:terminated_at="None"
    # accessipv4=""
    # accessipv6=""
    # addresses="public=208.113.135.196, 2607:f298:5:101d:f816:3eff:fe25:d05"
    # config_drive="True"
    # created="2016-03-18T04:35:57Z"
    # flavor="gp1.semisonic (50)"
    # hostid="2a74b475939b7e5956a2334e4ce2f5675340e0976778b53742aaec99"
    # id="9d5e5a03-5820-48e3-9bdf-f5ac18af4c64"
    # image="CentOS-7 (c1e8c5b5-bea6-45e9-8202-b8e769b661a4)"
    # key_name="rack1"
    # name="delete2"
    # os-extended-volumes:volumes_attached="[]"
    # progress="0"
    # properties=""
    # security_groups="[{u'name': u'default'}]"
    # status="ACTIVE"
    # tenant_id="ac8fccd077d04729b4cb5f85506bb336"
    # updated="2016-03-18T04:36:07Z"
    # user_id="b55909ea28574f479dc9cf5a9b673697"

  end

  private

  # find a flavor matching the requested type
  def self.find_flavor(endpoint, match)
    favors(endpoint).keys.first rescue []
  end

  # find an image matching the requested type
  def self.find_image(endpoint, match)
    images(endpoint).keys.first rescue []
  end

  # retrieve the flavors
  def self.flavors(endpoint)
    raw = base(endpoint, "openstack flavors list", "-f csv")

    o = []
    return o

  end

  # retrieve the images
  def self.images(endpoint)
    raw = base(endpoint, "openstack images list", "-f csv")
    o = []
    return o
  end

  # provide the base CLI interface with correct flags
  def self.base(endpoint, cmd, with_result=nil)

    log("OpenStack inputs #{endpoint}")
    cmd = "openstack --os-username \'#{endpoint['os-username']}\' " \
                    "--os-password \'#{endpoint['os-password']}\' " \
                    "--os-project-name \'#{endpoint['os-project-name']}\' " \
                    "--os-region-name \'#{endpoint['os-region-name']}\' " \
                    "--os-auth-url \'#{endpoint['os-auth-url']}\' " \
                    "#{cmd}"

    o = nil
    if with_result
      o = %x[#{cmd} #{with_result}] rescue nil
    else
      o = system(cmd) rescue false
    end
    log "executed OpenStack command #{cmd}"

    return o

  end

  def self.public_v4(raw)
    if raw =~ /public=([0-9.]),/
      return $0
    else
      raw
    end
  end

  # parse raw shell output
  def self.unshell(raw)

    o = {}
    raw.each do |line|
      l = line.split('=')
      o[l[0]] = l[1]
    end
    return o
  end

  # parse raw cvs voutput
  def self.uncsv(raw, key)

    header = raw[0].split(",")
    raw.delete[0]
    o = {}
    raw.each do |line|
      l = line.split(",")
      oo = {}
      header.each_index do |i,value|
        oo[value] = l[i]
      end
      o[oo[key]] = oo
    end
    return o

  end

  #   "ID","Name"
  # "149ae890-a138-4a35-9ad1-d9c31aa3750c","CoreOS"
  def self.match_image(images, request)
    images.each do |k, v|
      return k if v =~ request
    end
    # backup to CentOS
    images.each do |k, v|
      return k if v =~ /CentOS/
    end
    # if all else fails, return first image
    return images.keys.first
  end

  # "ID","Name","RAM","Disk","Ephemeral","Swap","VCPUs","RXTX Factor","Is Public","Extra Specs"
  # "100","gp1.subsonic",1024,80,0,"",1,1.0,True,""
  def self.match_flavor(flavors, request)
    # exact match, easy!
    return request if flavors[request]

    # try to match RAM
    flavors.each do |k, values|
      return k if values["RAM"] == request
    end

    # assume % input, pick matching item
    percent = ((request.to_i/100)*flavors.length).to_i
    return favors.keys[percent] rescue flavors.keys.first

  end

end