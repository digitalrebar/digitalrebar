class Chef
  class Node
    # All information about configured nets is stored in
    # self[:crowbar_wall][:network][:nets] as rows in a matrix.
    # Each row has the following entries:
    # row[0] = The name of the network.
    # row[1] = The name of the range within that network.
    # row[2] = The address assigned from the range.  nil means no address.
    # row[3] = The interface hierarchy that the address is assigned to.
    #          This is an array, and the first interface in the array
    #          is the one that most people will be interested in.
    def addresses(net="admin",type=::IP,range=nil)
      res = []
      (self[:crowbar_wall][:network][:nets] || [] rescue []).each do |row|
        next unless net.nil? || row[0] == net
        next unless range.nil? || row[1] == range
        res << ::IP.coerce(row[2])
      end
      res.select{|a|a.kind_of? type}
    end
    def all_addresses(type=::IP)
      addresses(nil,type,nil)
    end
    def address(net="admin",type=::IP,range=nil,exact=false)
      res = self.addresses(net,type,range).first
      return res if res || exact
      (::IP.coerce("#{self[:crowbar][:network][net][:address]}/#{self[:crowbar][:network][net][:netmask]}") rescue nil) ||
        ::IP.coerce(self[:ipaddress])
    end
    def interfaces(net="admin",range=nil)
      res = nil
      (self[:crowbar_wall][:network][:nets] || [] rescue []).each do |row|
        next unless net.nil? || row[0] == net
        next unless range.nil? || row[1] == range
        ifs = row[3].map{|n|::Nic.new(n)}
        next unless res.nil? || res == ifs
        res = ifs
      end
      res
    end
    def interface(net="admin",range=nil)
      self.interfaces(net,range).first
    end
  end
end
