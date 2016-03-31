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
    images = images(endpoint)
    image_id = match_image(images, image || "CentOS")
    flavors = flavors(endpoint)
    flavor_id = match_flavor(flavors, flavor || "2048")
    nets = networks(endpoint)
    pub_net = public_net(nets)
    pri_net = private_net(nets)
    first_net = nets.keys.first
    params = "--key-name \'#{keyname}\' " \
             "--image \'#{image_id}\' " \
             "--flavor \'#{flavor_id}\' "
    params += "--nic \'net-id=#{pub_net}\' " if pub_net
    params += "--nic \'net-id=#{pri_net}\' " if pri_net
    params += "--nic \'net-id=#{first_net}\' " if !pri_net and !pub_net
    params += "\'#{name}\'"
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
    log "OpenStack removed key '#{name}'"
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
    log "OpenStack get server #{id} is #{o["name"]} + #{o["id"]}"
    log "OpenStack get DEBUG #{o.inspect}" if os_debug(endpoint)
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
    log "OpenStack DEBUG flavors #{o.inspect}" if os_debug(endpoint)
    return o
  end

  # retrieve the images
  def self.images(endpoint)
    raw = base(endpoint, "image list", "-f csv")
    o = uncsv(raw, "ID")
    log "OpenStack DEBUG images #{o.inspect}" if os_debug(endpoint)
    return o
  end

  # use neutron to get the networks (eventually, this should be in OpenStack API)
  def self.networks(endpoint)

    o = netbase endpoint, "net-list", "-f csv"
    r = uncsv(o, "id")
    # HACK remove networks without active subnets - detect because the only have a 36 char subnet GUID
    r.delete_if { |v, k| k["subnets"].size < 40}
    log "OpenStack networks DEBUG hash: #{r.inspect}" if os_debug(endpoint)
    return r

  end

  def self.netbase(endpoint, cmd, with_result=nil)

    full_cmd = "neutron --os-username \'#{endpoint['os-username']}\' " \
                    "--os-password \'#{endpoint['os-password']}\' " \
                    "--os-tenant-name \'#{endpoint['os-project-name']}\' " \
                    "--os-region-name \'#{endpoint['os-region-name']}\' " \
                    "--os-auth-url \'#{endpoint['os-auth-url']}\' " \
                    "#{cmd}"

    o = nil
    if with_result
      raw = %x[#{full_cmd} #{with_result}] rescue "ERROR: Command not executed"
      unless os_debug(endpoint)
        log "OpenStack Neutron executed command [openstack #{cmd} #{with_result}] > '#{raw[0..40]}'"
      else
        log "OpenStack Neutron DEBUG executed command\n#{full_cmd} #{with_result}"
        log "OpenStack Neutron DEBUG raw result\n#{raw}" 
      end
      case with_result
      when "-f csv"
        o = raw.gsub("\",\"","|||").gsub("\"","").split("\r\n")
      when "-f shell"
        o = raw.split("\n")
      else
        o = raw.split("\n")
      end
      log "OpenStack Neutron DEBUG result\n#{o}" if os_debug(endpoint)
    else
      o = system(full_cmd) rescue false
      log "system call OpenStack Neutron command [openstack #{cmd}] with result #{o}"
    end
    return o
  end

  # provide the base CLI interface with correct flags
  def self.base(endpoint, cmd, with_result=nil)

    full_cmd = "openstack --os-username \'#{endpoint['os-username']}\' " \
                    "--os-password \'#{endpoint['os-password']}\' " \
                    "--os-project-name \'#{endpoint['os-project-name']}\' " \
                    "--os-region-name \'#{endpoint['os-region-name']}\' " \
                    "--os-auth-url \'#{endpoint['os-auth-url']}\' " \
                    "#{cmd}"

    o = nil
    if with_result
      raw = %x[#{full_cmd} #{with_result}] rescue "ERROR: Command not executed"
      unless os_debug(endpoint)
        log "OpenStack executed command [openstack #{cmd} #{with_result}] > '#{raw.truncate(40)}'"
      else
        log "OpenStack DEBUG executed command\n#{full_cmd} #{with_result}"
        log "OpenStack DEBUG raw result\n#{raw}" 
      end
      case with_result
      when "-f csv"
        o = raw.gsub("\",\"","|||").gsub("\"","").split("\r\n")
      when "-f shell"
        o = raw.split("\n")
      else
        o = raw.split("\n")
      end
      log "OpenStack DEBUG result\n#{o}" if os_debug(endpoint)
    else
      o = system(full_cmd) rescue false
      log "system call OpenStack command [openstack #{cmd}] with result #{o}"
    end

    return o

  end

  # to handle variation w/ OpenStack clouds, we have to reolve several possible names for public or private networks
  def self.network(nets, names)
    nets.each do |key, net|
      names.each do |name|
        return key if net["name"] =~ /#{name}/im
        return key if net[:name] =~ /#{name}/im
      end
    end
    return nil
  end

  def self.addresses(raw)
    nics = raw.split(";")
    o = {}
    nics.each do |nic|
      parse = nic.split("=")
      name = parse.first.strip
      addresses = parse[1].split(",")
      v4 = nil
      v6 = nil
      addresses.each do |addr|
        v4 = addr.strip if addr =~/\./
        v6 = addr.strip if addr =~/\:/
      end
      o[name] = {"v4": v4, "v6": v6, "name": name}
    end
    log "OpenStack addresses #{o} from #{raw}"
    return o
  end

  # to handle variation w/ OpenStack clouds, we have to reolve several possible names for public or private networks
  # order should be in likehood of correct match
  def self.private_net(nets)
    return network(nets, %w[private internal servicenet priv])
  end

  # to handle variation w/ OpenStack clouds, we have to reolve several possible names for public or private networks
  # order should be in likehood of correct match
  def self.public_net(nets)
    return network(nets, %w[public ext-net external pub ext])
  end

  # parse raw shell output
  def self.unshell(raw)

    o = {}
    raw.each do |line|
      l = line.split('=', 2)
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

  def self.os_debug(endpoint)
    return true if endpoint['os-debug'] and endpoint['os-debug']=="true"
    return false
  end

  #   "ID","Name"
  # "149ae890-a138-4a35-9ad1-d9c31aa3750c","CoreOS"
  def self.match_image(images, request)
    if request
      images.each do |k, v|
        return k if v["Name"] =~ /#{request}/
      end
    end
    # ok, maybe they were too specific - try w/ less
    if request
      images.each do |k, v|
        return k if v["Name"] =~ /#{request[0..5]}/
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