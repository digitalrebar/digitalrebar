# Copyright 2013, Dell 
# 
# Licensed under the Apache License, Version 2.0 (the "License"); 
# you may not use this file except in compliance with the License. 
# You may obtain a copy of the License at 
# 
#  http://www.apache.org/licenses/LICENSE-2.0 
# 
# Unless required by applicable law or agreed to in writing, software 
# distributed under the License is distributed on an "AS IS" BASIS, 
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
# See the License for the specific language governing permissions and 
# limitations under the License. 
# 
#

# Base class to represent IP4 and IP6 addresses.
# This class winds up delegating most of its work to
# the IP::IP4 and IP::IP6 classes, which cannnot be directly created.
# We strongly prefer to use CIDR address notation.
# We use this in preference to IPAddr because IPAddr
# has a few non-CIDR assumptions.
class IP
  include Comparable
  protected

  # Translate a CIDR subnet specification into a bitfield
  # representing a netmask.
  def subnet_to_mask
    ((1 << @subnet) - 1) << (self.class::BITS - @subnet)
  end

  # Translate the address component of an IP address into
  # an array consisting of the parts of the address.
  def to_a
    bits = @address
    res = []
    self.class::PARTS.times do
      res << (bits & self.class::PART_MASK)
      bits >>= self.class::BITS_PER_PART
    end
    res.reverse
  end

  # The IP4 and IP6 classes use the same initialization function.
  # We handle 2 cases:
  # One argument with the address in string form in CIDR format:
  #   IP4: '127.0.0.1/8'
  #   IP6: '::1/128'
  # The first argument as an integer representaion of the address and an
  # optional subnet as the second argument.
  #
  # In both cases, if the subnet is omitted it is assumed to be the numerically
  # largest possible one for the address type, effectivly creating a single
  # address range.
  def initialize(*a)
    if a[0].kind_of?(String)
      self.address = a[0]
    elsif a[0].kind_of?(Integer)
      unless self.class::RANGE.include?(a[0].abs)
        raise RangeError.new("Address #{a[0]} out of range for #{self.class.name}")
      end
      @address = a[0].abs
      if a[1].kind_of?(Integer)
        unless (0..self.class::BITS).include?(a[1])
          raise RangeError.new("Subnet #{a[1]} out of range for #{self.class.name}")
        end
        @subnet = a[1]
      else
        @subnet = self.class.BITS
      end
    elsif a[0].kind_of?(self.class)
      @address = a[0].address
      @subnet = a[0].subnet
    else
      raise ArgumentError.new("Cannot create #{self.class.name} address out of #{a[0].inspect}")
    end
  end

  public

  # Coerce something into an IP4 or IP6 address, if possible.
  def self.coerce(*a)
    return a[0] if a[0].kind_of?(IP)
    [::IP::IP4,::IP::IP6].each do |klass|
      o = klass.new(*a) rescue false
      return o if o
    end
    raise ArgumentError.new("#{a.inspect} cannot be coerced into an IP address")
  end

  # Bootstrap the rest of the methods Comparable provides.
  def <=>(other)
    other = ::IP.coerce(other)
    case
    when v6? && other.v4? then -1
    when other.v6? && v4? then 1
    else @address <=> other.address
    end
  end

  # We will need this for mathy operators.
  def to_i
    @address.abs
  end
  alias address to_i

  # This + Comparable lets us build and use Ranges out of IP addresses.
  def succ
    self.class.new((@address + 1), @subnet)
  end

  # Test to see if two addresses are in the same network.
  def include?(other)
    other = self.class.coerce(other)
    raise ArgumentError.new("#{self.inspect} is not the same class as #{other.inspect}") unless other.class == self.class
    (self.network..self.broadcast) === other
  end

  alias === include?

  # Give us a nicer printed representation.
  def inspect
    "#<#{self.class.name}: #{to_s}>"
  end

  def subnet
    @subnet
  end

  def subnet=(subn)
    unless subn.to_i <= self.class::BITS
      raise RangeError.new("#{name} subnets must be numbers <= #{self.class::BITS}")
    end
    @subnet = subn.to_i
    self
  end

  def addr
    to_s.split('/')[0]
  end

  # Get the network address for this address.
  def network
    self.class.new(@address & subnet_to_mask(), @subnet)
  end

  # Get the broadcast address for this address
  def broadcast
    self.network + ((1 << (self.class::BITS - @subnet)) - 1)
  end

  # Set a new address for this object.
  def address=(address)
    if address.kind_of?(String)
      @address,s = self.class.parse_address(address)
      if s
        @subnet = s.to_i
      elsif @subnet.nil?
        @subnet = self.class::BITS
      end
    elsif address.kind_of?(Integer)
      address = address.abs
      raise RangeError.new("#{address} is out of range for #{self.class}") unless
        self.class.RANGE.include?(address)
      @address = address
    else
      raise ArgumentError.new("#{address} cannot be coerced into an IP address")
    end
    self
  end

  def v4?
    false
  end

  def v6?
    false
  end

  # Anything else, assume we want mathy goodness.
  def method_missing(m,*args,&block)
    self.class.new(case
                   when (args and block_given?) then @address.send(m,*args,&block)
                   when block_given? then @address.send(m,&block)
                   when args then @address.send(m,*args)
                   else @address.send(m)
                   end.abs,
                   @subnet)
  end

  class IP4 < IP
    BITS=32
    PARTS=4
    RANGE=(0...(1 << BITS))
    BITS_PER_PART=8
    PART_MASK=(1 << BITS_PER_PART) - 1
    MATCH_RE=/^(\d{1,3}\.){3}\d{1,3}$/

    private
    @address = nil
    @subnet = nil

    # Translate a netmask into a subnet.
    # We explicitly only care about CIDR compatible netmasks,
    # and will die horribly if someone wants to use a holey subnet.
    def self.netmask_to_subnet(mask)
      bits = mask.split('.').inject(0){|acc,i| acc = (acc << 8) + i.to_i}
      res = 32
      while bits[0] == 0
        res-=1
        bits >>= 1
      end
      while bits[0] == 1
        bits >>= 1
      end
      if bits > 0
        raise ArgumentError.new("#{mask} cannot be converted into a CIDR subnet!")
      end
      res
    end

    # Parse a string into an IP4 address or die trying
    def self.parse_address(a)
      addr,subnet = a.split('/',2)
      if addr.kind_of?(String) && (addr =~ MATCH_RE)
        addr = addr.split('.').map do |i|
          i = i.to_i
          if i >= 256
            raise RangeError.new("#{i} is too big for an IP4 address!")
          end
          i
        end.inject(0){|acc,i| acc = (acc << 8) + i}
      else
        raise ArgumentError.new("#{addr} is not a valid IP4 address!")
      end
      if subnet.kind_of?(String)
        if subnet =~ MATCH_RE
          subnet = self.netmask_to_subnet(subnet)
        elsif subnet =~ /^\d+$/ && (subnet.to_i <= 32)
          subnet = subnet.to_i
        else
          raise RangeError.new("#{subnet} is not a valid IP4 subnet!")
        end
      else
        subnet = 32
      end
      [addr,subnet]
    end

    public

    # Return the netmask in string format for our subnet
    def netmask
      bits = ~ ((1 << (BITS - @subnet)) - 1)
      res = []
      PARTS.times do
        res << (bits & PART_MASK)
        bits >>= BITS_PER_PART
      end
      res.reverse.join('.')
    end

    # Set our new subnet based on the passed netmask
    def netmask=(mask)
      @subnet = self.class.netmask_to_subnet(mask)
      self
    end

    # Return out address in CIDR format.
    def to_s
      "#{to_a().join('.')}/#{@subnet}"
    end

    # Return our address in reverse DNS lookup format.
    def reverse
      "#{to_a.reverse.join('.')}.in-addr.arpa"
    end

    def reachable?
      system("ping -c 1 -w 1 -q #{self.addr}")
    end

    def v4?
      true
    end

  end

  class IP6 < IP
    BITS=128
    PARTS=8
    RANGE=(0...(1 << BITS))
    BITS_PER_PART=16
    PART_MASK=(1 << BITS_PER_PART) - 1
    private
    @address = nil
    @subnet = nil

    # Parse an IPv6 address or die trying.
    # Parsing an IP6 address is fun due to its canonical representation.
    def self.parse_address(a)
      addr,subnet = a.split('/',2)
      unless addr.kind_of?(String) && (addr =~ /^[0-9a-f:]+$/) &&
          (! addr.include?(':::')) && addr.length >= 2
        raise ArgumentError.new("#{addr} is not a valid IP6 address")
      end
      if addr.include?('::')
        # Handle some degenerate cases first
        addr = "0" + addr if addr[0..1] == '::'
        addr << "0" if addr[-2..-1] == '::'
        # By now, addr must at least equal '0::0'
        addr = addr.split('::')
        unless addr.length == 2 # only one '::' allowed!
          raise ArgumentError.new("Only one :: allowed in an IP6 address!")
        end
        addr[0] = addr[0].split(':')
        addr[2] = addr[1].split(':')
        unless (addr[0].length + addr[2].length) <= PARTS
          raise ArgumentError.new("#{addr} has too many parts!")
        end
        addr[1] = Array.new((PARTS - (addr[0].length + addr[2].length)),"0")
        addr.flatten!
      else
        addr = addr.split(':')
      end
      unless addr.length == PARTS # An IP6 address has 8 elements
        raise RangeError.new("#{addr} is incorrectly formatted.")
      end
      unless subnet.nil? || subnet.to_i <= BITS
        raise RangeError.new("#{subnet} is out of range for an IP6 address.")
      end
      addr = addr.map do|i|
        i = i.hex
        if i > PART_MASK
          raise RangeError.new("#{'%x' % i} is too big.")
        else
          i
        end
      end.inject(0){|acc,i| acc = (acc << BITS_PER_PART) + i}
      [addr, subnet]
    end

    # Return the address component in canonical form.
    def canonical_address
      f = 0
      in_run = false
      runs = Array.new
      a = self.to_a
      a.each_index do |i|
        if a[i] == 0 && i < (PARTS - 1)
          f = i unless in_run
          in_run = true
        elsif in_run && (i == 7 || a[i].nonzero?)
          len = i - f
          runs[len] = [f,i] if len > 1 && runs[len].nil?
          in_run = false
        end
      end
      len = runs.length - 1
      return a.map{|i| '%x' % i}.join(':') if len == -1
      f,l = runs[len]
      res = a[0...f].map{|i| '%x' % i}.join(':') + '::'
      unless a[7].zero?
        res += a[l..7].map{|i| '%x' % i}.join(':')
      end
      res
    end

    public

    # Print our address in CIDR format
    def to_s
      "#{canonical_address}/#{@subnet}"
    end

    # Print our address in reverse DNS format.
    def reverse
      bits = @address
      res = []
      32.times do
        res << '%x' % (bits & 15)
        bits >>= 4
      end
      res.join('.') + ".ip6.arpa"
    end

    def reachable?
      system("ping6 -c 1 -w 1 -q #{self.addr}")
    end

    def v6?
      true
    end

  end
end
