class Disk

  @@syspath = "/sys/block"
  @@by_id = "/dev/disk/by-id"
  @@id_sort_keys = [ /\/ata-/,
                     /\/scsi-[a-zA-Z]/,
                     /\/scsi-[^1]/,
                     /\/scsi-/,
                     /\/cciss-/,
                     /\/wwn-/,
                     /\/usb-/ ]
  def self.update(node)
    node.set[:crowbar_wall] = {} unless node[:crowbar_wall]
    node.set[:crowbar_wall][:reservations] = {} unless node[:crowbar_wall][:reservations]
    present_disks = all.sort.map do |ent|
      ent.to_hash
    end
    Chef::Log.info("Current disks: #{present_disks.inspect}")
    recorded_disks = node[:crowbar_wall][:reservations][:disks] || []
    Chef::Log.info("Recorded disks: #{recorded_disks.inspect}")
    disks = {}
    # Record the current set of disks
    present_disks.each_index do |i|
      d = present_disks[i]
      disks[d["unique_name"]]= { "current" => d, "index" => i }
    end
    recorded_disks.each do |d|
      found_id = (disks.keys & d.ids).first
      if found_id
        # One of our ids is already used as a unique name.  Hooray.
        disks[found_id]["recorded"] = d
      elsif disks.key?(d["unique_name"])
        # We don't have an ID that matches, but our unique name does.
        # Hopefully they are referring to the same device
        disks[d["unique_name"]]["recorded"] = d
      end
      # If we get through the if without hitting anything, this disk has vanished.
    end
    new_disks = disks.values.sort{|a,b| a["index"] <=> b["index"]}.map do |val|
      if val.key?("recorded")
        val["recorded"].merge(val["current"])
      else
        val["current"]
      end
    end
    node.set[:crowbar_wall][:reservations][:disks] = new_disks
    claims = node[:crowbar_wall][:reservations][:claimed_disks]
    if claims && !claims.empty?
      claims = claims.to_hash
      claims.delete_if{|k,v|!new_disks.any?{|d|d['unique_name'] == k}}
      node.set[:crowbar_wall][:reservations][:claimed_disks] = claims
    end
  end

  def self.all
    Dir.glob("/sys/block/*").map do |ent|
      new(ent)
    end.reject do |ent|
      ent.virtual || ent.readonly
    end
  end

  def initialize(dev)
    @dev = dev.split("/")[-1]
    bpath = nil
    [ @@syspath, @@by_id ].each do |base|
      next unless File.symlink?("#{base}/#{@dev}")
      bpath = base

    end
    raise "#{dev} is not a device" unless bpath
    if bpath == @@by_id
      @dev = File.readlink("#{bpath}/#{@dev}").split("/")[-1]
    end
    @basepath = "#{@@syspath}/#{@dev}"
  end

  def ids
    pathfind(@@by_id).sort do |a,b|
      id_sort_key(a) <=> id_sort_key(b)
    end
  end

  def path
    File.expand_path(File.readlink(@basepath),@@syspath)
  end

  def unique_name
    ids.push("/dev/#{@dev}").first
  end

  def <=>(other)
    pp = self.pathparts
    opp = other.pathparts
    [pp.length, pp] <=> [opp.length, opp]
  end

  def removable
    sysread("removable",:bool)
  end

  def readonly
    sysread("ro",:bool)
  end

  def ssd
    !sysread("queue/rotational",:bool)
  end

  def size
    sysread("size",:integer) * 512
  end

  def usb
    pathparts.any?{|c|/^usb/.match(c)}
  end

  def virtual
    path.split("/").any?{|ent|ent =="virtual"}
  end

  def partitions
    Dir.glob("#{@basepath}/#{@dev}[1-9]*")
  end

  def partitioned
    !partitions.empty?
  end

  def mkgpt
    system("parted -a optimal #{unique_name} mklabel gpt") unless partitioned
  end

  def held
    !Dir.glob("#{@basepath}/holders/*").empty?
  end

  def formatted
    %x{blkid -u filesystem -s UUID /dev/#{@dev}}.strip.length > 0
  end

  def to_hash
    { "ids" => ids,
      "unique_name" => unique_name,
      "path" => path,
      "usb" => usb,
      "readonly" => readonly,
      "ssd" => ssd,
      "size" => size,
      "removable" => removable,
      "virtual" => virtual,
      "partitioned" => partitioned,
      "held" => held,
      "formatted" => formatted,
      "used" => held || partitioned || formatted
    }
  end

  def owner(node)
    (node[:crowbar_wall][:reservations][:claimed_disks][unique_name] || nil) rescue nil
  end

  def own(node,claimant)
    node.set[:crowbar_wall] = {} unless node[:crowbar_wall]
    node.set[:crowbar_wall][:reservations] = {} unless node[:crowbar_wall][:reservations]
    h = node[:crowbar_wall][:reservations][:claimed_disks] ? node[:crowbar_wall][:reservations][:claimed_disks].to_hash : {}
    h[unique_name] = claimant
    node.set[:crowbar_wall][:reservations][:claimed_disks] = h
  end

  def disown(node)
    node.set[:crowbar_wall] = {} unless node[:crowbar_wall]
    node.set[:crowbar_wall][:reservations] = {} unless node[:crowbar_wall][:reservations]
    node.set[:crowbar_wall][:reservations][:claimed_disks] = {} unless node[:crowbar_wall][:reservations][:claimed_disks]
    disks = node[:crowbar_wall][:reservations][:claimed_disks].to_hash
    disks.reject!{|k,v| k == unique_name}
    node.set[:crowbar_wall][:reservations][:claimed_disks] = disks
  end

  protected

  def id_sort_key(dsk_id)
    idx = @@id_sort_keys.index{|re|re.match(dsk_id)}
    idx ||= 999
    Chef::Log.info([idx,dsk_id].inspect)
    return [idx,dsk_id]
  end

  def sysread(file,convert=:string)
    data = IO.read(File.join(@basepath,file)).strip
    case convert
    when :bool then data != "0"
    when :integer then data.to_i
    when :string then data
    end
  end

  def pathparts
    File.readlink(@basepath).split("/")[2..-3]
  end

  def pathfind(prefix)
    Dir.glob("#{prefix}/*").select do |ent|
      File.symlink?(ent) && (File.readlink(ent).split("/")[-1] == @dev)
    end
  end
end
