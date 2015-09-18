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

class Network < ActiveRecord::Base

  ADMIN_CATEGORY = "admin"
  BMC_CATEGORY   = "bmc"
  V6AUTO         = "auto"   # if this changes, update the :v6prefix validator too!
  DEFAULTCONDUIT = '1g0'
  BMCCONDUIT     = 'bmc'

  audited

  validate        :check_network_sanity
  after_commit    :add_role, on: :create
  after_save      :auto_prefix
  before_destroy  :remove_role
  after_commit :on_create_hooks, on: :create
  after_commit :on_change_hooks, on: :update
  after_commit :on_destroy_hooks, on: :destroy

  validates_format_of :v6prefix, :with=>/auto|([a-f0-9]){1,4}:([a-f0-9]){1,4}:([a-f0-9]){1,4}:([a-f0-9]){1,4}/, :message => I18n.t("db.v6prefix", :default=>"Invalid IPv6 prefix."), :allow_nil=>true

  has_many :network_ranges,       :dependent => :destroy
  has_many :network_allocations,  :through => :network_ranges
  has_one  :network_router,       :dependent => :destroy

  alias_attribute :ranges,      :network_ranges
  alias_attribute :router,      :network_router
  alias_attribute :allocations, :network_allocations

  scope :in_category,   ->(c) { where(:category => c) }

  belongs_to :deployment

  def self.lookup_network(ipstring, category = "admin")
    addr = IP.coerce(ipstring)
    Network.in_category(category).each do |n|
      n.ranges.where(overlap: false).each do |r|
        return n if r === addr
      end
    end
    Network.find_by!(name: "unmanaged-internal")
  end

  def self.address(params)
    raise "Must pass a hash of args" unless params.kind_of?(Hash)
    network = Network.find_by!(name: params[:network])
    range = network.ranges.find_by!(name: params[:range])
    range.network_allocations.where(node_id: params[:node]).first
  end

  def self.make_global_v6prefix
    prefix_array = []
    raw_prefix_array = (["fc".hex] + IO.read("/dev/random",5).unpack("C5"))
    3.times do |i|
      a = raw_prefix_array.pop
      a += (raw_prefix_array.pop << 8)
      prefix_array << a
    end
    prefix_array.reverse.map{|a|sprintf('%04x',a)}.join(':')
  end

  def self.check_conduit_sanity(a,b)
    hash_a = {}
    hash_b = {}
    a.split(',').each do |e|
      hash_a[e.strip] = true
    end
    b.split(',').each do |e|
      hash_b[e.strip] = true
    end
    # Conduits are allowed to overlap perfectly or not at all.
    return hash_a == hash_b || (hash_a.keys & hash_b.keys).empty?
  end

  def self.check_sanity(n)
    res = []
    # First, check the conduit to be sure it is sane.
    intf_re =  /^bmc|^([-+?]?)(\d{1,3}[mg])(\d+)$/
    if n.conduit.nil? || n.conduit.empty?
      res << [:conduit, "Conduit definition cannot be empty"]
      intfs = []
    else
      intfs = n.conduit.split(",").map{|intf|intf.strip}
      ok_intfs, failed_intfs = intfs.partition{|intf|intf_re.match(intf)}
      unless failed_intfs.empty?
        res << [:conduit, "Invalid abstract interface names in conduit: #{failed_intfs.join(", ")}"]
      end
      if ok_intfs.length > 1
        matches = ok_intfs.map{|intf|intf_re.match(intf)}
        tmpl = matches[0]
        if ! matches.all?{|i|(i[1] == tmpl[1]) && (i[2] == tmpl[2])}
          res << [:conduit, "Not all abstract interface names have the same speed and flags: #{n.conduit}"]
        end
      end
    end

    # Check to see that requested VLAN information makes sense.
    if n.use_vlan && !(1..4094).member?(n.vlan)
      res << [:vlan, "VLAN #{n.vlan} not sane"]
    end

    # Check to see if our requested teaming makes sense.
    if n.use_team
      if intfs.length < 2
        res << [:conduit, "Want bonding, but requested conduit #{n.conduit} has too few members"]
      elsif intfs.length > 8
        res << [:conduit, "Want bonding, but requested conduit #{n.conduit} has too many members"]
      end
      res << [:team_mode, "Invalid bonding mode"] unless (0..6).member?(n.team_mode)
    elsif intfs.length > 1
      # Conduit can only contain one abstract interface if we don't want bonding.
      res << [:conduit, "Do not want bonding, but requested conduit #{n.conduit} has multiple members"]
    end

    # Should be obvious, but...
    unless n.name && !n.name.empty?
      res << [:name, "No name"]
    end
    res
  end

  def template_cleaner(a)
    res = {}
    a.each do |k,v|
      next if k.to_s == "id" || k.to_s.match(/_id$/)
      res[k] = v.kind_of?(Hash) ? template_cleaner(v) : v
    end
    res
  end

  def to_template
    res = template_cleaner(attributes)
    res[:ranges] = ranges.map{|r|template_cleaner(r.attributes)}
    if router
      res[:router] = template_cleaner(router.attributes)
    end
    res
  end

  def auto_ranges(node)
    res = []
    if node.is_admin? && ranges.exists?(name: "admin")
      res << ranges.find_by(name: "admin")
    else
      res << ranges.find_by(name: "host")
    end
    res << ranges.find_by(name: "host-v6")
    res.compact
  end

  def auto_allocate(node)
    auto_ranges(node).map do |range|
      range.allocate(node)
    end
  end

  def role
    bc = Barclamp.where(name: "network").first
    bc.roles.where(name: "network-#{name}").first
  end

  def node_allocations(node)
    allocations.node(node).map{|a|a.address}.sort
  end

  def make_node_role(node)
    nr = nil
    NodeRole.transaction do
      # do we have an existing NR?
      nr = NodeRole.where(:node_id => node.id, :role_id => role.id).first
      # if not, we have to create one
      nr ||= role.add_to_node(node)
    end
    nr
  end

  private

  def create_auto_v6_range
    if v6prefix && !NetworkRange.find_by(name: "host-v6", network_id: id)
      NetworkRange.create!(name: "host-v6",
                           first: "#{v6prefix}::1/64",
                           last:  ((IP.coerce("#{v6prefix}::/64").broadcast) - 1).to_s,
                           network_id: id)
    end
  end

  # for auto, we add an IPv6 prefix
  def auto_prefix
    # Add our IPv6 prefix.
    if (v6prefix == V6AUTO)
      Role.logger.info("Network: Creating automatic IPv6 prefix for #{name}")
      user = User.admin.first
      # this config code really needs to move to Rebar base
      cluster_prefix = user.settings(:network).v6prefix[name]
      if cluster_prefix.nil? or cluster_prefix.eql? V6AUTO
        cluster_prefix = Network.make_global_v6prefix
        user.settings(:network).v6prefix[name] = cluster_prefix
      end
      Network.transaction do
        update_column("v6prefix", sprintf("#{cluster_prefix}:%04x",id))
      end
      Rails.logger.info("Network: Created #{sprintf("#{cluster_prefix}:%04x",id)} for #{name}")
      create_auto_v6_range
    end
  end

  # every network needs to have a matching role and auto v6 range
  def add_role
    role_name = "network-#{name}"
    unless Role.exists?(name: role_name)
      Rails.logger.info("Network: Adding role and attribs for #{role_name}")
      bc = Barclamp.find_key "network"
      Role.transaction do
        create_auto_v6_range
        r = Role.find_or_create_by!(name: role_name,
                                    type: "BarclampNetwork::Role",   # force
                                    jig_name: Rails.env.production? ? "chef" : "test",
                                    barclamp_id: bc.id,
                                    description: I18n.t('automatic_by', :name=>name),
                                    library: false,
                                    implicit: true,
                                    milestone: true,    # may need more logic later, this is safest for first pass
                                    bootstrap: false,   # don't bootstrap networks anymore.
                                    discovery: false  ) # don't discovery networks anymore.
        RoleRequire.create!(:role_id => r.id, :requires => "network-server")
        # The admin net must be bound before any other network can be bound.
        RoleRequireAttrib.create!(role_id: r.id, attrib_name: 'network_interface_maps')
        RoleRequireAttrib.create!(role_id: r.id, attrib_name: 'network-current-config')
        RoleRequireAttrib.create!(role_id: r.id, attrib_name: 'network-wanted-config')
        # attributes for jig configuraiton
        Attrib.create!(:role_id => r.id,
                         :barclamp_id => bc.id,
                         :name => "#{role_name}_addresses",
                         :description => "#{name} network addresses assigned to a node",
                         :map => "rebar/network/#{name}/addresses")
        Attrib.create!(:role_id => r.id,
                         :barclamp_id => bc.id,
                         :name => "#{role_name}_targets",
                         :description => "#{name} network addresses to be used as ping test targets",
                         :map => "rebar/network/#{name}/targets")
        Attrib.create!(:role_id => r.id,
                         :barclamp_id => bc.id,
                         :name => "#{role_name}_conduit",
                         :description => "#{name} network conduit map for this node",
                         :map => "rebar/network/#{name}/conduit")
        Attrib.create!(:role_id => r.id,
                         :barclamp_id => bc.id,
                         :name => "#{role_name}_resolved_conduit",
                         :description => "#{name} network interfaces used on this node",
                         :map => "rebar/network/#{name}/resolved_interfaces")
        Attrib.create!(:role_id => r.id,
                         :barclamp_id => bc.id,
                         :name => "#{role_name}_vlan",
                         :description => "#{name} network vlan tag",
                         :map => "rebar/network/#{name}/vlan")
        Attrib.create!(:role_id => r.id,
                         :barclamp_id => bc.id,
                         :name => "#{role_name}_team_mode",
                         :description => "#{name} network bonding mode",
                         :map => "rebar/network/#{name}/team_mode")
        Attrib.create!(:role_id => r.id,
                         :barclamp_id => bc.id,
                         :name => "#{role_name}_use_vlan",
                         :description => "Whether the #{name} network should use a tagged VLAN interface",
                         :map => "rebar/network/#{name}/use_vlan")
        Attrib.create!(:role_id => r.id,
                         :barclamp_id => bc.id,
                         :name => "#{role_name}_use_team",
                         :description => "Whether the #{name} network should bond its interfaces",
                         :map => "rebar/network/#{name}/use_team")
        Attrib.create!(:role_id => r.id,
                         :barclamp_id => bc.id,
                         :name => "#{role_name}_use_bridge",
                         :description => "Whether #{name} network should create a bridge for other barclamps to use",
                         :map => "rebar/network/#{name}/use_bridge")
        # attributes for hints
        # These belong to the barclamp, not the role.
        Attrib.create!(:barclamp_id => bc.id,
                       :name => "hint-#{name}-v4addr",
                       :description => "Hint for #{name} network to assign v4 IP address",
                       :map => "#{name}-v4addr",
                       :schema => {
                         "type" => "str",
                         "required" => true,
                         "pattern" => '/([0-9]{1,3}\.){3}[0-9]{1,3}/'})
        Attrib.create!(:barclamp_id => bc.id,
                       :name => "hint-#{name}-v6addr",
                       :description => "Hint for #{name} network to assign v6 IP address",
                       :map => "#{name}-v6addr",
                       :schema => {
                         "type" => "str",
                         "required" => true,
                         "pattern" => '/[0-9a-f:]+/'})
      end
    end
  end

  def remove_role
    role = self.role
    role.destroy! if role  # just in case the role was lost, we still want to be able to delete
    # Also destroy the hints
    ["v4addr","v6addr"].each do |n|
      Attrib.destroy_all(name: "hint-#{name}-#{n}")
    end
  end

  def check_network_sanity

    Network.check_sanity(self).each do |err|
      errors.add(err[0], "Network #{name}: #{err[1]}")
    end

    # Check to see that this network's conduits either overlap perfectly or not at all
    # with conduits on other networks.  Ranges are not considered here.'
    Network.all.each do |net|
      unless Network.check_conduit_sanity(conduit, net.conduit)
        errors.add(:conduit, "Network #{name}: Conduit mapping overlaps with network #{net.name}")
      end
    end if conduit

    # We also must have a deployment
    unless deployment
      errors.add(:deployment_id, "Network #{name}: Cannot create a network without binding it to a deployment")
    end

  end

  # Call the on_network_delete hooks.
  def on_destroy_hooks
    # do the low cohorts last
    Rails.logger.info("Network: calling all role on_network_delete hooks for #{name}")
    Role.all_cohorts_desc.each do |r|
      begin
        Rails.logger.info("Network: Calling #{r.name} on_network_delete for #{self.name}")
        r.on_network_delete(self)
      rescue Exception => e
        Rails.logger.error "Network #{name} attempting to cleanup role #{r.name} failed with #{e.message}"
      end
    end
  end

  # Call the on_network_change hooks.
  def on_change_hooks
    # do the low cohorts last
    Rails.logger.info("Network: calling all role on_network_change hooks for #{name}")
    Role.all_cohorts_desc.each do |r|
      begin
        Rails.logger.info("Network: Calling #{r.name} on_network_change for #{self.name}")
        r.on_network_change(self)
      rescue Exception => e
        Rails.logger.error "Network #{name} attempting to change role #{r.name} failed with #{e.message}"
      end
    end
  end

  def on_create_hooks
    # Call all role on_network_create hooks with self.
    # These should happen synchronously.
    # do the low cohorts first
    Rails.logger.info("Network: calling all role on_network_create hooks for #{name}")
    Role.all_cohorts.each do |r|
      Rails.logger.info("Network: Calling #{r.name} on_network_create for #{self.name}")
      r.on_network_create(self)
    end
  end

end
