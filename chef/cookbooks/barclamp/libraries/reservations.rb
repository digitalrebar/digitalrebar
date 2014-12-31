class Chef
  class Node
    def disks
      res = Mash.new
      self[:crowbar_ohai][:disks][:order].each do |d|
        res[d] = self[:crowbar_ohai][:disks][d]
      end
      res
    end

    def reserve_disk(name,owner)
      self.normal[:crowbar_wall] ||= Mash.new
      self.normal[:crowbar_wall][:reservations] ||= Mash.new
      self.normal[:crowbar_wall][:reservations][:disks] ||= Mash.new
      name = name.split("/")[-1]
      natural_name = if self[:crowbar_ohai][:disks][name]
                       name
                     elsif self[:crowbar_ohai][:disks][:by_id][name]
                       self[:crowbar_ohai][:disks][:by_id][name]
                     elsif self[:crowbar_ohai][:disks][:by_path][name]
                       self[:crowbar_ohai][:disks][:by_path][name]
                     else
                       raise "Cannot find natural name for disk #{name}"
                     end
      disk = self[:crowbar_ohai][:disks][natural_name]
      unless disk[:available]
        Chef::Log.error("Disk #{name} is not available to be reserved!")
        return false
      end
      unique_name = disk["preferred_device_name"].split("/")[-1]
      return true if self[:crowbar_wall][:reservations][:disks][unique_name] == owner
      if self[:crowbar_wall][:reservations][:disks][unique_name]
        Chef::Log.error("Disk #{name} is already claimed by #{self[:crowbar_wall][:reservations][:disks][unique_name]}")
        return false
      end
      self.normal[:crowbar_wall][:reservations][:disks][unique_name] = owner
      true
    end

    def reserved_disks
      res = Mash.new
      (self.normal[:crowbar_wall][:reservations][:disks] rescue {}).each do |k,v|
        res[v] ||= Array.new
        res[v] << k
      end
      res
    end

    def reserved_disk?(name)
      name = name.split("/")[-1]
      natural_name = if self[:crowbar_ohai][:disks][name]
                       name
                     elsif self[:crowbar_ohai][:disks][:by_id][name]
                       self[:crowbar_ohai][:disks][:by_id][name]
                     elsif self[:crowbar_ohai][:disks][:by_path][name]
                       self[:crowbar_ohai][:disks][:by_path][name]
                     else
                       raise "Cannot find natural name for disk #{name}"
                     end
      disk = self[:crowbar_ohai][:disks][natural_name]
      unique_name = disk["preferred_device_name"].split("/")[-1]
      self[:crowbar_wall][:reservations][:disks][unique_name] rescue nil
    end

    def release_disk(name,owner)
      name = name.split("/")[-1]
      natural_name = if self[:crowbar_ohai][:disks][name]
                       name
                     elsif self[:crowbar_ohai][:disks][:by_id][name]
                       self[:crowbar_ohai][:disks][:by_id][name]
                     elsif self[:crowbar_ohai][:disks][:by_path][name]
                       self[:crowbar_ohai][:disks][:by_path][name]
                     else
                       raise "Cannot find natural name for disk #{name}"
                     end
      disk = self[:crowbar_ohai][:disks][natural_name]
      unique_name = disk["preferred_device_name"].split("/")[-1]
      unless self[:crowbar_wall][:reservations][:disks][unique_name] == owner
        Chef::Log.error("Cannot release disk #{name}, it is not owned by #{owner}")
        return false
      end
      self.normal[:crowbar_wall][:reservations][:disks] = self[:crowbar_wall][:reservations][:disks].reject{|k,v|k == unique_name}
      true
    end
  end
end
