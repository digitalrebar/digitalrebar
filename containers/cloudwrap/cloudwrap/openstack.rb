#!/usr/bin/env ruby
# Copyright 2016, RackN Inc

# endpoint for OpenStack
# {
#   os-username: '',
#   os-password: '',
#   os-project-name: '',
#   os-auth-url: '',
#   os-region-name: ''
#   os-debug: 'false'
#   os-network-internal: 'auto'
#   os-network-external: 'auto'
# }

module OpenStack

  # create server
  def self.create(endpoint, name, keyname, image, flavor)
    images = images(endpoint)
    image_id = match_image(images, image || "CentOS")
    image_id = match_image(images, "CentOS") unless image_id 
    image_id = match_image(images, "Ubuntu") unless image_id 
    unless image_id
      log "OpenStack WARNING Could not find image naming #{image}, using first from list"
      image_id = images.keys.first
    end
    flavors = flavors(endpoint)
    flavor_id = match_flavor(flavors, flavor || "2048")
    nets = networks(endpoint)
    pub_net = public_net(endpoint, nets)
    pri_net = private_net(endpoint, nets)
    first_net = nets.keys.first
    params = "--key-name \'#{keyname}\' " \
             "--image \'#{image_id}\' " \
             "--flavor \'#{flavor_id}\' "
    params += "--nic \'net-id=#{pub_net}\' " if pub_net
    params += "--nic \'net-id=#{pri_net}\' " if pri_net
    params += "--nic \'net-id=#{first_net}\' " if !pri_net and !pub_net
    params += "\'#{name}\'"
    o = base endpoint, "server create #{params}"
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
    base endpoint, "server delete \'#{id}\'", false
  end

  # reboot provided server
  def self.reboot(endpoint, id)
    base endpoint, "server reboot --hard \'#{id}\'"
  end

  # get list of servers
  def self.list(endpoint)
    raw = base(endpoint, "server list")
    o = unarray raw, "ID"
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
    o = base(endpoint, "server show \'#{id}\'")
    log "OpenStack get server #{id} is #{o["name"]} + #{o["id"]}"
    log "OpenStack get DEBUG #{o.inspect}" if os_debug(endpoint)
    return o
  end

  private

  # retrieve the flavors
  def self.flavors(endpoint)
    raw = base(endpoint, "flavor list")
    o = unarray raw, "ID"
    log "OpenStack DEBUG flavors #{o.inspect}" if os_debug(endpoint)
    return o
  end

  # retrieve the images
  def self.images(endpoint)
    raw = base(endpoint, "image list")
    o = unarray raw, "ID"
    o.delete_if { |v, k| !(k["Status"] =~ /active/i) }
    log "OpenStack DEBUG images #{o.inspect}" if os_debug(endpoint)
    return o
  end

  # get the networks
  def self.networks(endpoint)

    raw = base endpoint, "network list"
    r = unarray raw, "ID"
    # get details on each network to make better decisions (slow, but required)
    detail_map = ["admin_state_up", "status", "router_extenal"]
    r.each do |key, values|
      detail = base endpoint, "network show \'#{key}\'"
      detail_map.each { |d| r[key][d] = detail[d] }
    end
    # remove networks that are not active with subnets 
    r.delete_if { |v, k| !(k["status"] =~ /active/i) or !(k["admin_state_up"] =~ /up/i) or k["Subnets"].gsub(/[a-f0-9-]+ /, '').size < 10 }
    log "OpenStack networks DEBUG hash: #{r.inspect}" if os_debug(endpoint)
    return r

  end

  # provide the base CLI interface with correct flags
  def self.base(endpoint, cmd, with_result=true)

    full_cmd = "openstack --os-username \'#{endpoint['os-username']}\' " \
                    "--os-password \'#{endpoint['os-password']}\' " \
                    "--os-project-name \'#{endpoint['os-project-name']}\' " \
                    "--os-region-name \'#{endpoint['os-region-name']}\' " \
                    "--os-auth-url \'#{endpoint['os-auth-url']}\' " \
                    "#{cmd}"
    full_cmd += " -f json" if with_result   

    o = nil
    raw = %x[#{full_cmd}] rescue "ERROR: Command not executed"
    unless os_debug(endpoint)
      log "OpenStack executed command [openstack #{cmd} > '#{raw.truncate(40)}'"
    else
      log "OpenStack DEBUG executed command\n#{full_cmd}"
      log "OpenStack DEBUG raw result\n#{raw}" 
    end
    # we always ask for a JSON result if we wanted a result
    o = (JSON.parse(raw) rescue {}) if with_result
    log "OpenStack DEBUG result\n#{o.inspect}" if with_result and os_debug(endpoint)
    return o

  end

  # to handle variation w/ OpenStack clouds, we have to reolve several possible names for public or private networks
  def self.network(nets, names)
    nets.each do |key, net|
      names.each do |name|
        return key if net["name"] =~ /#{name}/im
        return key if net["Name"] =~ /#{name}/im
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
  def self.private_net(endpoint, nets)
    case endpoint["os-network-internal"]
    when 'none'
      return nil
    when 'auto', nil, ''
      return network(nets, %w[private internal servicenet priv]) || nets.keys.first
    else
      return network(nets, endpoint["os-network-internal"].split(/,|\s/))
    end
  end

  # to handle variation w/ OpenStack clouds, we have to reolve several possible names for public or private networks
  # order should be in likehood of correct match
  def self.public_net(endpoint, nets)

    case endpoint["os-network-external"]
    when 'none'
      return nil
    when 'auto', nil, ''
      external = {}
      nets.each do |k, v|
        unless v["router_external"]
          external[k] = base endpoint, "network show \'#{k}\'"
        end
        external[k] = v if v["router_external"]
      end
      case external.length
      when 0  # no externals, use original list based on name
        return network(nets, %w[public ext-net external default pub ext])
      when 1  # one external, use it
        return external.keys.first
      else # several external, try name match
        return network(external, %w[public ext-net external default pub ext])
      end
    else
      return network(nets, endpoint["os-network-external"].split(/,|\s/))
    end

  end

  # turn arrays into hashes w/ IDs
  def self.unarray(raw, key)
    o = {}
    raw.each { |item| o[item[key]] = item }
    return o
  end

  def self.os_debug(endpoint)
    return (endpoint['os-debug'] and endpoint['os-debug']=="true") rescue false
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
      return k if values["RAM"] == (request.to_s rescue nil)
      return k if values["RAM"] == (request.to_i rescue nil)
      return k if (values["RAM"].to_s rescue nil) == request
    end

    # assume % input, pick matching item    
    percent = ((request.to_i/100)*flavors.length).to_i
    return favors.sort_by{ |k, v| v["RAM"].to_i }[percent] rescue flavors.keys.first

  end

end