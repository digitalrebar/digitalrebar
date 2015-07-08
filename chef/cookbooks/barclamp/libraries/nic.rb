# Library of routines for handling network interfaces

# Base class representing a network interface.
class ::Nic
  include Comparable
  private
  @@interfaces = Hash.new
  @nic = nil
  @nicdir = nil
  @addresses = ::Array.new
  @dependents = nil

  # Helper method for reading values from sysfs for a nic.
  def sysfs(file)
    ::File.read("#{@nicdir}/#{file}").strip
  end

  def sysfs_put(file,val)
    ::File.open("#{@nicdir}/#{file}","a+") do |f|
      f.syswrite(val.to_s)
    end
  end

  # Basic initialization routine for subclasses of Nic.
  def initialize(nic)
    @nic = nic.dup.freeze
    @nicdir = "/sys/class/net/#{nic}".freeze
    refresh()
  end

  # Helper for running ip commands.
  def run_ip(arg)
    ::Kernel.system("ip #{arg}")
  end

  def enable_ipv6
    ::Kernel.system("sysctl net.ipv6.conf.#{@nic}.disable_ipv6=0")
  end

  # Update nic info
  def self.update(node)
    node.set[:crowbar_wall] = {} unless node[:crowbar_wall]
    node.set[:crowbar_wall][:reservations] = {} unless node[:crowbar_wall][:reservations]

    # Check for lldpcli and if that isn't found try the lldpctl (older form)
    cmd=%x{which lldpcli}.chomp
    if cmd and cmd != ""
      cmd="#{cmd} show neighbors -f keyvalue"
    else
      cmd=%x{which lldpctl}.chomp
      if cmd and cmd != ""
        cmd="#{cmd} -f keyvalue"
      end
    end
    if cmd and cmd != ""
      # Mark all nics up so that we can eventually get all the switch info
      nics.each do |nic|
        nic.up
      end

      # Get the lldp data we have.
      pi={}
      data = `#{cmd}`
      data.split("\n").each do |line|
        parts = line.split("=", 2)
        key = parts[0]
        value = parts[1]

        keys = key.split(".")
        pos = pi
        keys.each do |k|
          break if k == 'unknown-tlvs'   # Skip painful data for now
          if k == keys.last
            if pos[k]
              arr = [ pos[k], value ]
              pos[k] = arr.flatten
            else
              pos[k] = value
            end
          else
            pos[k] = {} unless pos[k]
          end
          pos = pos[k]
        end
      end

      pi[:status] = "Success"
      node.set[:crowbar_wall][:reservations][:ports] = pi
    else
      node.set[:crowbar_wall][:reservations][:ports] = { :status => "No lldpcli" }
    end

  end

  # Return an unsorted array of all nics on the system.
  def self.__nics
    res = []
    ::Dir.entries("/sys/class/net").each do |d|
      next if d == '.' or d == '..'
      next unless ::File.directory?("/sys/class/net/#{d}")
      res << Nic.new(d)
    end
    res
  end

  public

  # Return an array of all the nics present on the system.
  # This array will be sorted in nic dependency order.
  def self.nics
    res = Array.new
    Nic.__nics.each do |nic|
      len = nic.dependents.length
      res[len] ||= Array.new
      res[len] << nic
    end
    res.compact.map{|r| r.sort}.flatten
  end

  def self.refresh_all
    @@interfaces.each_value{|n|n.refresh}
  end

  # Some class functions for determining what kind of nic
  # we are looking at.
  def self.exists?(nic)
    nic.kind_of?(::Nic) or
      ::File.exists?("/sys/class/net/#{nic}")
  end

  def self.coerce(nic)
    return nic if nic.kind_of?(::Nic)
     ::Nic.new(nic)
  end

  def self.bridge?(nic)
    nic.kind_of?(::Nic::Bridge) or
      ::File.exists?("/sys/class/net/#{nic}/bridge/bridge_id")
  end

  def self.bond?(nic)
    nic.kind_of?(::Nic::Bond) or
      ::File.exists?("/sys/class/net/#{nic}/bonding/slaves")
  end

  def self.vlan?(nic)
    nic.kind_of?(::Nic::Vlan) or
      ::File.exists?("/proc/net/vlan/#{nic}")
  end

  # ifindex and iflink track parent -> child relationships
  # among nics.  We only really use it for vlan nics for now.
  def ifindex
    sysfs("ifindex").to_i
  end

  def iflink
    sysfs("iflink").to_i
  end

  # We only have this to reduce the number of times we have to call
  # ip to get the addresses for an interface.  If we can get this
  # info in a more efficient way (via an ioctl or whatever) it can go away.
  def refresh
    @addresses = ::Array.new
    @dependents = nil
    ::IO.popen("ip -o addr show dev #{@nic}") do |f|
      f.each do |line|
        parts = line.gsub('\\','').split
        next unless parts[2] =~ /^inet/
        next if parts[5] == 'link'
        addr = IP.coerce(parts[3])
        @addresses << addr
      end
    end
    self
  end

  # Get a list of all IP4 and IP6 addresses bound to a nic.
  def addresses
    @addresses
  end

  # IP address manipulation routines
  def add_address(addr)
    addr = ::IP.coerce(addr).dup.freeze
    return self if @addresses.include?(addr)
    enable_ipv6 if addr.v6?
    if run_ip("addr add #{addr.to_s} dev #{@nic}")
      @addresses << addr
      self
    else
      raise ::RuntimeError.new("Could not add #{addr.to_s} to #{@nic}")
      false
    end
  end

  def remove_address(addr)
    addr = ::IP.coerce(addr)
    return self unless @addresses.include?(addr)
    unless run_ip("addr del #{addr.to_s} dev #{@nic}")
      raise ::RuntimeError.new("Could not remove #{addr.to_s} from #{@nic}.")
    end
    @addresses.delete(addr)
    self
  end

  # Check for the PID of the dhcp agent managing this interface, if any.
  def dhcp_pid
    res = %x{pgrep -f 'dh(client|cpcd).*#{@nic}'}.strip
    res.empty? ? nil : res.to_i
  end

  # This kills all IP addresses and routes set to go through a nic.
  # Use with caution.
  def flush
    self.dhcp_pid && kill_dhcp && sleep(3)
    run_ip("-4 route flush dev #{@nic}")
    run_ip("-6 route flush dev #{@nic}")
    run_ip("addr flush dev #{@nic} scope global")
    @addresses = ::Array.new
    self
  end

  # Kill any attached DHCP agent attached to this nic.
  # If a PID is killed, the interface is also flushed to
  # get rid of stale addresses and routes.
  def kill_dhcp
    pid = dhcp_pid
    return unless pid
    system("kill #{pid}")
  end

  # Several helper routines for querying the state of a nic.

  # Does this nic have a cable plugged in to it?
  def link_up?
    sysfs("carrier") == "1"
  end

  def iftype
    sysfs("type").to_i
  end

  # Get the mac address of a nic.
  def mac
    sysfs("address") rescue "00:00:00:00:00:00"
  end

  # Get the speed a this nic is operating at.
  # May not always be accurate.
  def speed
    sysfs("speed").to_i rescue 0
  end

  # Get and set the MTU for an interface.
  def mtu
    sysfs("mtu").to_i rescue 0
  end

  def mtu=(mtu)
    run_ip("link set #{@nic} mtu #{mtu}")
  end

  def flags
    sysfs("flags").hex
  end

  # Is this nic configured to be up?
  def up?
    (flags & 1) > 0
  end

  # Is this nic in broadcast mode?
  def broadcast?
    (flags & 2) > 0
  end

  def debug?
    (flags & 4) > 0
  end

  # Is this a loopback interface?
  def loopback?
    (flags & 8) > 0
  end

  # Is this nic a pointtopoint interface?
  def pointtopoint?
    (flags & 16) > 0
  end

  # Is this nic operating in notrailers mode?
  def notrailers?
    (flags & 32) > 0
  end

  # Is this nic operating?
  def running?
    (flags & 64) > 0
  end

  # Is this nic in noarp mode?
  def noarp?
    (flags & 128) > 0
  end

  # Is this nic in promiscuous mode?
  def promiscuous?
    (flags & 256) > 0
  end

  def allmulti?
    (flags & 512) > 0
  end

  # Is this nic a master (i.e, a bond)?
  def master?
    (flags & 1024) > 0
  end

  # Is this nic enalaved to something else?
  def slave?
    (flags & 2048) > 0
  end

  def multicast?
    (flags & 4096) > 0
  end

  def portsel?
    (flags & 8192) > 0
  end

  def automedia?
    (flags & 16384) > 0
  end

  def dynamic?
    (flags & 32786) > 0
  end

  # Set a nic to be up.
  def up
    return self if up?
    run_ip("link set #{@nic} up")
    self
  end

  # Set a nic to be down.
  def down
    return self unless up?
    run_ip("link set #{@nic} down")
    self
  end

  # Get the name of this nic.
  def name
    @nic
  end

  # The next two routines onlt really work for interfaces that
  # are link-basd subinterfaces -- i.e, vlan interfaces and the like.
  # They do not return useful information for bonds and bridges.
  # Get this nic's parents based on ifindex -> iflink matching.
  def parents
    return [] if self.ifindex == self.iflink
    self.class.__nics.select do |n|
      (n.ifindex == self.iflink) && ! (n == self)
    end
  end

  # Ditto, except get children instead.
  def children
    self.class.__nics.select do |n|
      (n.iflink == self.ifindex) && ! ( n == self )
    end
  end

  # Get the slaves of this interface.  Unless you are a bond or a bridge,
  # you don't have any.  This is here promarily to make the sort logic
  # a little simpler.
  def slaves
    []
  end

  # Return the bond we are enslaved to, or nil if we are not in a bond.
  def bond_master
    return nil unless File.exists?("#{@nicdir}/master")
    Nic.new(File.readlink("#{@nicdir}/master").split('/')[-1])
  end

  # Return the bridge we are enslaved to, or nil if we are not in a bridge.
  def bridge_master
    return nil unless File.exists?("#{@nicdir}/brport/bridge")
    Nic.new(File.readlink("#{@nicdir}/brport/bridge").split('/')[-1])
  end

  # Return the interface we are enslaved to, if we are enslaved to something.
  def master
    self.bond_master || self.bridge_master
  end

  # Figure out all the interfaces we depend on.
  def dependents
    return @dependents if @dependents
    res = self.parents
    res.dup.each do |d|
      res = res + d.dependents
    end
    res = res + slaves
    slaves.each do |s|
      res = res + s.dependents
    end
    @dependents = res
    res
  end

  def <=>(other)
    case
    when self.name == other.name then 0
    when self.name == "lo" then -1
    when other.name == "lo" then 1
    when self.dependents.member?(other) then 1
    when other.dependents.member?(self) then -1
    else self.name <=> other.name
    end
  end

  def to_s
    @nic
  end

  # Base case for the destroy function.
  # Children of Nic should actually destroy themselves instead of
  # leaving themselves unconfigured and down.
  # Note that destroy also destroys any children of this nic.
  # Use with care.
  def destroy
    children.each do |child|
      child.destroy
    end
    master = self.master()
    master.remove_slave(self) if master
    self.flush
    self.down
    @@interfaces.delete(@nic)
  end

  # Override the usual new function for Nic.  All nic types shold
  # be created through Nic.new, and not through the subclasses.
  # Nic.new is intended to instantiate an object that tracks a nic
  # that is already present on the system.
  # If you want to create a new interface, call one of the
  # create methods on a subclass.
  def self.new(nic)
    if o = @@interfaces[nic]
      return o
    elsif vlan?(nic)
      o = ::Nic::Vlan.allocate
    elsif bridge?(nic)
      o = ::Nic::Bridge.allocate
    elsif bond?(nic)
      o = ::Nic::Bond.allocate
    elsif exists?(nic)
      o = ::Nic.allocate
    else
      raise ArgumentError.new("#{nic} does not exist!  Did you mean Nic.create?")
    end
    o.send(:initialize, nic)
    @@interfaces[nic] = o
    return o
  end

  # Base class for a bond.
  # We handle all bond manipulation via sysfs for maximum flexibility.
  class ::Nic::Bond < ::Nic
    MASTER='/sys/class/net/bonding_masters'

    private
    def self.kill_bond(nic)
      ::File.open(MASTER,"w") do |f|
        f.write("-#{nic}")
      end
    end

    def self.create_bond(nic)
      ::File.open(MASTER,"w") do |f|
        f.write("+#{nic}")
      end
      unless ::File.exists?("/sys/class/net/#{nic}")
        self.kill_bond(nic)
        raise ::RuntimeError.new("Could not create bond #{net}")
      end
    end

    public

    def slaves
      sysfs("bonding/slaves").split.map{|i| ::Nic.new(i)}
    end

    def add_slave(slave)
      unless ::Nic.exists?(slave)
        raise ::ArgumentError.new("#{slave} does not exist, cannot add to bond #{@nic}")
      end
      slave = Nic.coerce(slave)
      return slave if self.slaves.member?(slave)
      if current_master = slave.master()
        current_master.remove_slave(slave)
      end
      new_addrs = slave.addresses
      slave.flush
      slave.down
      new_addrs.each{|a|self.add_address a}
      sysfs_put("bonding/slaves","+#{slave}")
      slave
    end

    def remove_slave(slave)
      slave = self.class.coerce(slave)
      unless self.slaves.member?(slave)
        raise ::ArgumentError.new("#{slave} is not a member of bond #{@nic}")
      end
      sysfs_put("bonding/slaves","-#{slave}")
      slave
    end

    def mode
      sysfs("bonding/mode").split[1].to_i
    end

    def mode=(new_mode)
      begin
        self.down
        sysfs_put("bonding/mode",new_mode)
      ensure
        self.up
      end
      self
    end

    def miimon
      sysfs("bonding/miimon").to_i
    end

    def miimon=(millisecs)
      sysfs_put("bonding/miimon",millisecs)
      self
    end

    def down
      slaves.each{|s|s.down}
      super
    end

    def up
      super
      slaves.each{|s|s.up}
      self
    end

    def destroy
      slaves.each do |slave|
        remove_slave(slave)
      end
      super
      ::Nic::Bond.kill_bond(@nic)
      nil
    end

    def self.create(nic,mode=6,miimon=100)
      if self.exists?(nic)
        raise ::ArgumentError.new("#{nic} already exists.")
      elsif ! ::File.exists?("/sys/module/bonding")
        unless ::Kernel.system("modprobe bonding")
          raise ::RuntimeError.new("Unable to load bonding module.")
        end
        # Kill any bonds that were automatically created
        ::File.read(MASTER).strip.split.each do |i|
          self.kill_bond(i)
        end
      end
      self.create_bond(nic)
      iface = ::Nic.new(nic)
      iface.mode = mode
      iface.miimon = miimon
      iface.up
      iface
    end
  end

  # Base class for a bridge.  We handle most bridge manipulation via brctl.
  class ::Nic::Bridge < ::Nic
    def slaves
      ::Dir.entries("#{@nicdir}/brif").reject do |i|
        i == '.' || i == '..'
      end.map{|i| ::Nic.new(i)}
    end

    def add_slave(slave)
      slave = self.class.coerce(slave)
      unless ::Nic.exists?(slave)
        raise ::ArgumentError.new("#{slave} does not exist, cannot add to bridge#{@nic}")
      end
      return if self.slaves.member?(slave)
      if current_master = slave.master()
        current_master.remove_slave(slave)
      end
      slave.up
      new_addrs = slave.addresses
      slave.flush
      new_addrs.each{|a|self.add_address a}
      ::Kernel.system("brctl addif #{@nic} #{slave}")
    end

    def remove_slave(slave)
      slave = self.class.coerce(slave)
      unless self.slaves.member?(slave)
        raise ::ArgumentError.new("#{slave} is not a member of bridge #{@nic}")
      end
      ::Kernel.system("brctl delif #{@nic} #{slave}")
      slave.down
    end

    def stp
      sysfs("bridge/stp_state").to_i == 1
    end

    def stp=(val)
      sysfs_put("bridge/stp_state",
                case val
                when true then 1
                when false then 0
                else raise ::ArgumentError.new("Bridge STP state must be either true or false.")
                end)
      self
    end

    def forward_delay
      sysfs("bridge/forward_delay").to_i
    end

    def forward_delay=(delay)
      sysfs_put("bridge/forward_delay",delay)
      self
    end

    def up
      slaves.each{|s|s.up}
      super
    end

    def destroy
      slaves.each do |slave|
        remove_slave(slave)
      end
      super
      ::Kernel.system("brctl delbr #{@nic}")
      nil
    end

    def self.create(nic,slaves=[])
      if self.exists?(nic)
        raise ::ArgumentError.new("#{nic} already exists.")
      end
      unless ::File.exists?("/sys/module/bridge")
        ::Kernel.system("modprobe bridge")
      end
      ::Kernel.system("brctl addbr #{nic}")
      iface = ::Nic.new(nic)
      slaves.each do |slave|
        iface.add_slave slave
      end
      iface.up
      iface
    end
  end

  # Base class for a vlan nic.
  # All vlan nics must be created as link types on a parent nic.
  class ::Nic::Vlan < ::Nic
    def vlan
      ::IO.readlines("/proc/net/vlan/config").each do |line|
        line = line.split('|')
        next unless line[0].strip == @nic
        return line[1].strip.to_i
      end
    end

    def destroy
      super
      ::Kernel.system("vconfig rem #{@nic}")
      nil
    end

    def up
      parents.each{|p|p.up}
      super
    end

    def self.create(parent,vlan)
      nic = "#{parent}.#{vlan}"
      if self.exists?(nic)
        raise ::ArgumentError.new("#{nic} already exists.")
      end
      unless self.exists?(parent)
        raise ::ArgumentError.new("Parent #{parent} for #{nic} does not exist")
      end
      unless (0..4095).member?(vlan)
        raise ::RangeError.new("#{vlan} must be between 1 and 4095.")
      end
      unless ::File.exists?("/sys/module/8021q")
        ::Kernel.system("modprobe 8021q")
      end
      parent.up
      Kernel.system("vconfig set_name_type DEV_PLUS_VID_NO_PAD")
      Kernel.system("vconfig add #{parent} #{vlan}")
      n = ::Nic.new(nic)
      n.up
      n
    end
  end
end
