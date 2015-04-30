# Copyright 2011, Dell
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

# See if anything has changed since last time.  If not, do nothing.
last_zone = (node[:crowbar_wall][:dns] || Mash.new rescue Mash.new).dup.reject{|k,v|k == "serial"}
zone = node[:crowbar][:dns].dup
return if zone == last_zone

admin = node[:crowbar][:dns][:contact].gsub("@",".")
serial = (node[:crowbar_wall][:dns][:serial] rescue 0) + 1
zonefiles = [zone[:domain]]

# Arrange for the forward lookup zone to be created.
template "/etc/bind/db.#{zone[:domain]}" do
  source "db.erb"
  mode 0644
  owner "root"
  case node[:platform]
  when "ubuntu","debian" then group "bind"
  when "centos","redhat","suse" then group "named"
  end
  notifies :reload, "service[bind9]"
  variables(:zone => zone,
            :serial => serial,
            :admin => admin,
            :nameserver => "#{node.name}.")
end

def populate_soa_defaults(zone)
  [ :ttl,
    :serial,
    :slave_refresh,
    :slave_retry,
    :slave_expire,
    :negative_cache ].each do |k|
    zone[k] ||= node[:crowbar][:dns][k]
  end
  zone
end

# Arrange for reverse lookup zones to be created.
# Since there is no elegant method for doing this that takes into account
# CIDR or IPv6, do it the excessively ugly way and create one zone per IP.
hosts = (zone[:hosts].keys || [] rescue [])
hosts.sort.each do |hostname|
  host=zone[:hosts][hostname]
  [:ip4addr, :ip6addr].each do |addr|
    next unless host[addr]
    rev_zone=Mash.new
    populate_soa_defaults rev_zone
    rev_domain=IP.coerce(host[addr]).reverse
    rev_zone[:domain]=rev_domain
    rev_zone[:hosts] ||= Mash.new
    rev_zone[:hosts]["#{rev_domain}."] = Mash.new
    rev_zone[:hosts]["#{rev_domain}."][:pointer]= if hostname == "@"
                                                    "#{zone[:domain]}."
                                                  else
                                                    "#{hostname}"
                                                  end
    Chef::Log.debug "Processing zone: #{rev_zone.inspect}"
    template "/etc/bind/db.#{rev_domain}" do
      source "db.erb"
      mode 0644
      owner "root"
      notifies :reload, "service[bind9]"
      variables(:zone => rev_zone,
                :serial => serial,
                :admin => admin,
                :nameserver => "#{node.name}.")
    end
    zonefiles << rev_domain
  end
end

node.normal[:crowbar_wall] ||= Mash.new
node.normal[:crowbar_wall][:dns] = zone
node.normal[:crowbar_wall][:dns][:serial] = serial

Chef::Log.debug "Creating zone file for zones: #{zonefiles.inspect}"
template "/etc/bind/zone.#{zone[:domain]}" do
  source "zone.erb"
  mode 0644
  owner "root"
  case node[:platform]
  when "ubuntu","debian" then group "bind"
  when "centos","redhat","suse" then group "named"
  end
  notifies :reload, "service[bind9]"
  variables(:zones => zonefiles)
end

include_recipe "bind9::default"

# Update named.conf.crowbar to include the new zones.
template "/etc/bind/named.conf.crowbar" do
  source "named.conf.crowbar.erb"
  mode 0644
  owner "root"
  case node[:platform]
  when "ubuntu","debian" then group "bind"
  when "centos","redhat","suse" then group "named"
  end
  variables(:zone => zone[:domain])
  notifies :reload, "service[bind9]"
end
