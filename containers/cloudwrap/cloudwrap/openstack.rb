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
  def self.create(endpoint, name, keyname, image, flavor)
    endpoint["debug"] = true
    images = images(endpoint)
    image_id = match_image(images, image || "CentOS")
    flavors = flavors(endpoint)
    flavor_id = match_flavor(flavors, flavor || "2048")
    params = "--key-name \'#{keyname}\' " \
             "--image \'#{image_id}\' " \
             "--flavor \'#{flavor_id}\' " \
             "\'#{name}\'"
    raw = base endpoint, "server create #{params}", "-f shell"
    o = unshell(raw)
    id = o["id"]
    log "openstack created server #{name} as #{id}"
    return o rescue nil
  end

  # upload key
  def self.addkey(endpoint, name, key)
    log "OpenStack adding key #{key}"
    base endpoint, "keypair create --public-key \'#{key}\' \'#{name}\'"
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
    log "OpenStack.get server #{id} is #{o["name"]} + #{o["id"]}"
    log "OpenStack.get DEBUG #{o.inspect}" if endpoint["debug"]
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

  # retrieve the flavors
  def self.flavors(endpoint)
    raw = base(endpoint, "flavor list", "-f csv")
    o = uncsv raw, "ID"
    log "OpenStack DEBUG flavors #{o.inspect}" if endpoint["debug"]
    return o
  end

  # retrieve the images
  def self.images(endpoint)
    raw = base(endpoint, "image list", "-f csv")
    o = uncsv raw, "ID"
    log "OpenStack DEBUG images #{o.inspect}" if endpoint["debug"]
    return o
  end

  # provide the base CLI interface with correct flags
  def self.base(endpoint, cmd, with_result=nil)

    #log("OpenStack inputs #{endpoint}")
    full_cmd = "openstack --os-username \'#{endpoint['os-username']}\' " \
                    "--os-password \'#{endpoint['os-password']}\' " \
                    "--os-project-name \'#{endpoint['os-project-name']}\' " \
                    "--os-region-name \'#{endpoint['os-region-name']}\' " \
                    "--os-auth-url \'#{endpoint['os-auth-url']}\' " \
                    "#{cmd}"

    o = nil
    if with_result
      raw = %x[#{full_cmd} #{with_result}] rescue ""
      log "OpenStack DEBUG raw result\n#{raw}" if endpoint["debug"]
      log "executed OpenStack command [openstack #{cmd} #{with_result}]"
      case with_result
      when "-f csv"
        o = raw.gsub("\",\"","|||").gsub("\"","").split("\r\n")
      when "-f shell"
        o = raw.split("\n")
      else
        o = raw.split("\n")
      end
      log "OpenStack DEBUG result\n#{o}" if endpoint["debug"]
    else
      o = system(full_cmd) rescue false
      log "system call OpenStack command [openstack #{cmd}] with result #{o}"
    end

    return o

  end

  # use Status "addresses"
  def self.public_v4(raw)
    o= if raw =~ /public:([0-9.]*),/
      $1
    else
      raw
    end
    log "OpenStack v4 address #{o} from #{raw}"
    return o
  end

  # parse raw shell output
  def self.unshell(raw)

    o = {}
    raw.each do |line|
      # public is a hack because of multiple uses of EQUAL in OpenStack return
      l = line.gsub("public=","public:").split('=')
      o[l[0]] = l[1].gsub!(/\A"|"\Z/, '')
    end
    return o
  end

  # parse raw cvs voutput
  def self.uncsv(raw, key)

    header = raw[0].split("|||")
    raw.delete_at 0 
    o = {}
    raw.each do |line|
      l = line.split(/\|\|\||,/)
      oo = {}
      header.each_index do |i|
        oo[header[i]] = ( l[i] rescue "" )
      end
      o[oo[key]] = oo
    end
    return o

  end

  #   "ID","Name"
  # "149ae890-a138-4a35-9ad1-d9c31aa3750c","CoreOS"
  def self.match_image(images, request)
    if request
      images.each do |k, v|
        return k if v["Name"] =~ /#{request}/
      end
    end
    # backup to CentOS
    images.each do |k, v|
      return k if v["Name"] =~ /cent|Cent|CENT/
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
    return favors.sort_by{ |k, v| v["RAM"].to_i }[percent] rescue flavors.keys.first

  end

end