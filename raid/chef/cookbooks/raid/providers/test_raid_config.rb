# Copyright (c) 2013 Dell Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

#
# This section is used to setup a test pass of 
# the resource without needing to call through
# chef.
#
# All the "if __FILE__ == $0" is doing this.
#
$in_chef=true

if !$in_chef
  class TestResource
    def debug_flag 
      true
    end
    def problem_file 
      "profile.out"
    end
  end
  @new_resource = TestResource.new
  
  class MyHash < Hash
    def save 
    end
  end

  @node = MyHash.new
  def node
    @node
  end
  def action(cmd, &block)
  end
  def log(*args)
    puts args
  end

  class Chef
    class Log
      def self.info(*args)
        puts args
      end
      def self.error(*args)
        puts args
      end
      def self.warn(*args)
        puts args
      end
      def self.debug(*args)
        puts args
      end
    end
  end

  require "../libraries/raid_data.rb"
  require "../libraries/lsi_ircu.rb"
  require "../libraries/lsi_megacli.rb"
  require "../libraries/wsman_cli.rb"
  require "../libraries/xml_util.rb"
  require "raid_config.rb"

  

  #
  # Used to talk to WSMAN.
  #
  host = '192.168.124.320'
  user = 'root'
  password = 'cr0wBar!'
  port = 443
  node[:ipmi] = Hash.new
  node[:ipmi][:bmc_user] = user
  node[:ipmi][:bmc_password] = password
  node["crowbar_wall"] = Hash.new
  node["crowbar_wall"]["ipmi"] = Hash.new
  node["crowbar_wall"]["ipmi"]["address"] = host

  def assert_equal(a, b, msg)
    raise Exception.new("Test#{@test_cnt}: (V: #{a} E: #{b}) #{msg}") if a != b
  end

  def test(name, &block)
    @test_cnt = name
    begin
      yield
    rescue Exception => e
      puts e.message
      puts e.backtrace
    end
  end

  # Test Build_volumes
  def display_results(vols, errs)
    puts "GREG: vols = #{vols.length}"
    vols.each do |v|
      puts "  v = #{v.vol_name} #{v.raid_level}"
      v.members.each do |d|
        puts "    d = #{d.enclosure}:#{d.slot}"
      end
    end
    puts "GREG: errs = #{errs.length}"
    errs.each do |e|
      puts "  e = #{e}"
    end
  end

  a1_controller_no_disks = []
  c = Crowbar::RAID::Controller.new
  c.supported_raid_levels = [ :RAID1, :RAID1E, :RAID5, :JBOD ]
  c.product_name = "Fred Prime"
  a1_controller_no_disks << c

  a2_controller_no_disks = []
  c = Crowbar::RAID::Controller.new
  c.supported_raid_levels = [ :RAID1, :RAID1E, :RAID5, :JBOD ]
  c.product_name = "Fred Prime"
  a2_controller_no_disks << c
  c = Crowbar::RAID::Controller.new
  c.supported_raid_levels = [ :RAID10, :RAID50 ]
  c.product_name = "John Prime"
  a2_controller_no_disks << c

  a1_controller_3_disks = []
  c = Crowbar::RAID::Controller.new
  c.supported_raid_levels = [ :RAID1, :RAID1E, :RAID5, :JBOD ]
  c.product_name = "Fred Prime"
  count = 0
  3.times do 
    d = Crowbar::RAID::RaidDisk.new
    d.controller = c
    d.enclosure = 0
    d.slot = count
    count += 1
    c.disks << d
  end
  a1_controller_3_disks << c

  a1_controller_5_disks = []
  c = Crowbar::RAID::Controller.new
  c.supported_raid_levels = [ :RAID1, :RAID10, :RAID1E, :RAID5, :JBOD ]
  c.product_name = "Fred Prime"
  count = 0
  5.times do 
    d = Crowbar::RAID::RaidDisk.new
    d.controller = c
    d.enclosure = 0
    d.slot = count
    count += 1
    c.disks << d
  end
  a1_controller_5_disks << c

  a2_controller_3_disks = []
  c = Crowbar::RAID::Controller.new
  c.supported_raid_levels = [ :RAID1, :RAID1E, :RAID5, :JBOD ]
  c.product_name = "Fred Prime"
  count = 0
  3.times do 
    d = Crowbar::RAID::RaidDisk.new
    d.controller = c
    d.enclosure = 0
    d.slot = count
    count += 1
    c.disks << d
  end
  a2_controller_3_disks << c
  c = Crowbar::RAID::Controller.new
  c.supported_raid_levels = [ :RAID10, :RAID50, :JBOD, :RAIDK ]
  c.product_name = "Fred Prime"
  count = 0
  3.times do 
    d = Crowbar::RAID::RaidDisk.new
    d.controller = c
    d.enclosure = 0
    d.slot = count
    count += 1
    c.disks << d
  end
  a2_controller_3_disks << c

  a2_controller_5_disks = []
  c = Crowbar::RAID::Controller.new
  c.supported_raid_levels = [ :RAID1, :RAID1E, :RAID5, :JBOD ]
  c.product_name = "Fred Prime"
  count = 0
  5.times do 
    d = Crowbar::RAID::RaidDisk.new
    d.controller = c
    d.enclosure = 0
    d.slot = count
    count += 1
    c.disks << d
  end
  a2_controller_5_disks << c
  c = Crowbar::RAID::Controller.new
  c.supported_raid_levels = [ :RAID10, :RAID50, :JBOD, :RAIDK ]
  c.product_name = "Fred Prime"
  count = 0
  5.times do 
    d = Crowbar::RAID::RaidDisk.new
    d.controller = c
    d.enclosure = 0
    d.slot = count
    count += 1
    c.disks << d
  end
  a2_controller_5_disks << c

  a2_controller_7_disks = []
  c = Crowbar::RAID::Controller.new
  c.supported_raid_levels = [ :RAID1, :RAID1E, :RAID5, :JBOD ]
  c.product_name = "John Prime"
  count = 0
  7.times do 
    d = Crowbar::RAID::RaidDisk.new
    d.controller = c
    d.enclosure = 0
    d.slot = count
    count += 1
    c.disks << d
  end
  a2_controller_7_disks << c
  c = Crowbar::RAID::Controller.new
  c.supported_raid_levels = [ :RAID10, :RAID50, :JBOD, :RAIDK ]
  c.product_name = "Fred Prime"
  count = 0
  7.times do 
    d = Crowbar::RAID::RaidDisk.new
    d.controller = c
    d.enclosure = 0
    d.slot = count
    count += 1
    c.disks << d
  end
  a2_controller_7_disks << c

  config_1_volume_no_cont = {
    "volumes"=> {
      "0" => { "raid_level"=> "RAID1" , "disk_count"=> "remaining", "order"=> 1, "stripe_size" => 2048, "max_size" => 12 },
    }
  }

  test "1" do
    vols, errs = build_volumes(config_1_volume_no_cont, a1_controller_no_disks)
    assert_equal(vols.length, 0, "volumes created")
    assert_equal(errs.length, 1, "Incorrect errors")
    assert_equal(errs[0], "Volume 0 doesn't match any controller", "Missing error")
  end
 
  test "2" do
    vols, errs = build_volumes(config_1_volume_no_cont, a2_controller_no_disks)
    assert_equal(vols.length, 0, "volumes created")
    assert_equal(errs.length, 1, "Incorrect errors")
    assert_equal(errs[0], "Volume 0 doesn't match any controller", "Missing error")
  end

  test "3" do
    vols, errs = build_volumes(config_1_volume_no_cont, a1_controller_3_disks)
    assert_equal(vols.length, 2, "volumes created")
    assert_equal(errs.length, 0, "Incorrect errors")
    assert_equal(vols[0].members.length, 2, "Incorrect disks")
    assert_equal(vols[0].raid_level, :RAID1, "Incorrect raid_level")
    assert_equal(vols[0].size, 12, "Incorrect max size")
    assert_equal(vols[0].stripe_size, 2048, "Incorrect stripe size")
    assert_equal(vols[0].controller, a1_controller_3_disks[0], "Incorrect controller")
    assert_equal(vols[1].members.length, 1, "Incorrect disks")
    assert_equal(vols[1].raid_level, :JBOD, "Incorrect raid_level")
    assert_equal(vols[1].controller, a1_controller_3_disks[0], "Incorrect controller")
  end

  test "4" do
    vols, errs = build_volumes(config_1_volume_no_cont, a1_controller_5_disks)
    assert_equal(vols.length, 2, "volumes created")
    assert_equal(errs.length, 0, "Incorrect errors")
    assert_equal(vols[0].members.length, 2, "Incorrect disks")
    assert_equal(vols[0].raid_level, :RAID1, "Incorrect raid_level")
    assert_equal(vols[0].controller, a1_controller_5_disks[0], "Incorrect controller")
    assert_equal(vols[1].members.length, 3, "Incorrect disks")
    assert_equal(vols[1].raid_level, :JBOD, "Incorrect raid_level")
    assert_equal(vols[1].controller, a1_controller_5_disks[0], "Incorrect controller")
  end

  config_1_volume_no_cont2 = {
    "volumes"=> {
      "0" => { "raid_level"=> "RAID1" , "disk_count"=> "remaining", "order"=> 1, "controller"=> "cntrl1" },
    }
  }

  test "5" do
    vols, errs = build_volumes(config_1_volume_no_cont2, a1_controller_no_disks)
    assert_equal(vols.length, 0, "volumes created")
    assert_equal(errs.length, 1, "Incorrect errors")
    assert_equal(errs[0], "Volume 0 doesn't match any controller", "Missing error")
  end

  test "6" do
    vols, errs = build_volumes(config_1_volume_no_cont2, a2_controller_no_disks)
    assert_equal(vols.length, 0, "volumes created")
    assert_equal(errs.length, 1, "Incorrect errors")
    assert_equal(errs[0], "Volume 0 doesn't match any controller", "Missing error")
  end 

  test "7" do
    vols, errs = build_volumes(config_1_volume_no_cont2, a1_controller_3_disks)
    assert_equal(vols.length, 2, "volumes created")
    assert_equal(errs.length, 0, "Incorrect errors")
    assert_equal(vols[0].members.length, 2, "Incorrect disks")
    assert_equal(vols[0].raid_level, :RAID1, "Incorrect raid_level")
    assert_equal(vols[0].controller, a1_controller_3_disks[0], "Incorrect controller")
    assert_equal(vols[0].size, "MAX", "Incorrect max size")
    assert_equal(vols[0].stripe_size, nil, "Incorrect stripe size")
    assert_equal(vols[1].members.length, 1, "Incorrect disks")
    assert_equal(vols[1].raid_level, :JBOD, "Incorrect raid_level")
    assert_equal(vols[1].controller, a1_controller_3_disks[0], "Incorrect controller")
  end 

  test "8" do
    vols, errs = build_volumes(config_1_volume_no_cont2, a1_controller_5_disks)
    assert_equal(vols.length, 2, "volumes created")
    assert_equal(errs.length, 0, "Incorrect errors")
    assert_equal(vols[0].members.length, 2, "Incorrect disks")
    assert_equal(vols[0].raid_level, :RAID1, "Incorrect raid_level")
    assert_equal(vols[0].controller, a1_controller_5_disks[0], "Incorrect controller")
    assert_equal(vols[1].members.length, 3, "Incorrect disks")
    assert_equal(vols[1].raid_level, :JBOD, "Incorrect raid_level")
    assert_equal(vols[1].controller, a1_controller_5_disks[0], "Incorrect controller")
  end

  config_1_volume_cont1 = {
    "controllers"=> {
      "cntr1"=> { },
    },
    "volumes"=> {
      "0" => { "raid_level"=> "RAID1" , "disk_count"=> "remaining", "order"=> 1, "controller"=> "cntr1" },
    }
  }

  test "9" do
    vols, errs = build_volumes(config_1_volume_cont1, a1_controller_no_disks)
    assert_equal(vols.length, 0, "volumes created")
    assert_equal(errs.length, 1, "Incorrect errors")
    assert_equal(errs[0], "Volume 0 doesn't match any controller", "Missing error")
  end

  test "10" do
    vols, errs = build_volumes(config_1_volume_cont1, a2_controller_no_disks)
    assert_equal(vols.length, 0, "volumes created")
    assert_equal(errs.length, 1, "Incorrect errors")
    assert_equal(errs[0], "Volume 0 doesn't match any controller", "Missing error")
  end

  test "11" do
    vols, errs = build_volumes(config_1_volume_cont1, a1_controller_3_disks)
    assert_equal(vols.length, 2, "volumes created")
    assert_equal(errs.length, 0, "Incorrect errors")
    assert_equal(vols[0].members.length, 2, "Incorrect disks")
    assert_equal(vols[0].raid_level, :RAID1, "Incorrect raid_level")
    assert_equal(vols[0].controller, a1_controller_3_disks[0], "Incorrect controller")
    assert_equal(vols[1].members.length, 1, "Incorrect disks")
    assert_equal(vols[1].raid_level, :JBOD, "Incorrect raid_level")
    assert_equal(vols[1].controller, a1_controller_3_disks[0], "Incorrect controller")
  end

  test "12" do
    vols, errs = build_volumes(config_1_volume_cont1, a1_controller_5_disks)
    assert_equal(vols.length, 2, "volumes created")
    assert_equal(errs.length, 0, "Incorrect errors")
    assert_equal(vols[0].members.length, 2, "Incorrect disks")
    assert_equal(vols[0].raid_level, :RAID1, "Incorrect raid_level")
    assert_equal(vols[0].controller, a1_controller_5_disks[0], "Incorrect controller")
    assert_equal(vols[1].members.length, 3, "Incorrect disks")
    assert_equal(vols[1].raid_level, :JBOD, "Incorrect raid_level")
    assert_equal(vols[1].controller, a1_controller_5_disks[0], "Incorrect controller")
  end

  config_1_volume_cont2 = {
    "controllers"=> {
      "cntr1"=> { "raid_level_filter"=> [ "RAID1" ] },
    },
    "volumes"=> {
      "0" => { "raid_level"=> "RAID1" , "disk_count"=> "remaining", "order"=> 1, "controller"=> "cntr1" },
    }
  }

  test "13" do
    vols, errs = build_volumes(config_1_volume_cont2, a1_controller_no_disks)
    assert_equal(vols.length, 0, "volumes created")
    assert_equal(errs.length, 1, "Incorrect errors")
    assert_equal(errs[0], "Volume 0 doesn't match any controller", "Missing error")
  end

  test "14" do
    vols, errs = build_volumes(config_1_volume_cont2, a2_controller_no_disks)
    assert_equal(vols.length, 0, "volumes created")
    assert_equal(errs.length, 1, "Incorrect errors")
    assert_equal(errs[0], "Volume 0 doesn't match any controller", "Missing error")
  end

  test "15" do
    vols, errs = build_volumes(config_1_volume_cont2, a1_controller_3_disks)
    assert_equal(vols.length, 2, "volumes created")
    assert_equal(errs.length, 0, "Incorrect errors")
    assert_equal(vols[0].members.length, 2, "Incorrect disks")
    assert_equal(vols[0].raid_level, :RAID1, "Incorrect raid_level")
    assert_equal(vols[0].controller, a1_controller_3_disks[0], "Incorrect controller")
    assert_equal(vols[1].members.length, 1, "Incorrect disks")
    assert_equal(vols[1].raid_level, :JBOD, "Incorrect raid_level")
    assert_equal(vols[1].controller, a1_controller_3_disks[0], "Incorrect controller")
  end

  test "16" do
    vols, errs = build_volumes(config_1_volume_cont2, a1_controller_5_disks)
    assert_equal(vols.length, 2, "volumes created")
    assert_equal(errs.length, 0, "Incorrect errors")
    assert_equal(vols[0].members.length, 2, "Incorrect disks")
    assert_equal(vols[0].raid_level, :RAID1, "Incorrect raid_level")
    assert_equal(vols[0].controller, a1_controller_5_disks[0], "Incorrect controller")
    assert_equal(vols[1].members.length, 3, "Incorrect disks")
    assert_equal(vols[1].raid_level, :JBOD, "Incorrect raid_level")
    assert_equal(vols[1].controller, a1_controller_5_disks[0], "Incorrect controller")
  end
  
  config_1_volume_cont2r = {
    "controllers"=> {
      "cntr1"=> { "raid_level_filter"=> [ "RAID1" ] },
    },
    "volumes"=> {
      "0" => { "raid_level"=> "RAID1" , "disk_count"=> "3", "order"=> 1, "controller"=> "cntr1" },
    }
  }

  test "13r" do
    vols, errs = build_volumes(config_1_volume_cont2r, a1_controller_no_disks)
    assert_equal(vols.length, 0, "volumes created")
    assert_equal(errs.length, 1, "Incorrect errors")
    assert_equal(errs[0], "Volume 0 doesn't match any controller", "Missing error")
  end

  test "14r" do
    vols, errs = build_volumes(config_1_volume_cont2r, a2_controller_no_disks)
    assert_equal(vols.length, 0, "volumes created")
    assert_equal(errs.length, 1, "Incorrect errors")
    assert_equal(errs[0], "Volume 0 doesn't match any controller", "Missing error")
  end

  test "15r" do
    vols, errs = build_volumes(config_1_volume_cont2r, a1_controller_3_disks)
    assert_equal(vols.length, 2, "volumes created")
    assert_equal(errs.length, 0, "Incorrect errors")
    assert_equal(vols[0].members.length, 2, "Incorrect disks")
    assert_equal(vols[0].raid_level, :RAID1, "Incorrect raid_level")
    assert_equal(vols[0].controller, a1_controller_3_disks[0], "Incorrect controller")
    assert_equal(vols[1].members.length, 1, "Incorrect disks")
    assert_equal(vols[1].raid_level, :JBOD, "Incorrect raid_level")
    assert_equal(vols[1].controller, a1_controller_3_disks[0], "Incorrect controller")
  end

  test "16r" do
    vols, errs = build_volumes(config_1_volume_cont2r, a1_controller_5_disks)
    assert_equal(vols.length, 2, "volumes created")
    assert_equal(errs.length, 0, "Incorrect errors")
    assert_equal(vols[0].members.length, 2, "Incorrect disks")
    assert_equal(vols[0].raid_level, :RAID1, "Incorrect raid_level")
    assert_equal(vols[0].controller, a1_controller_5_disks[0], "Incorrect controller")
    assert_equal(vols[1].members.length, 3, "Incorrect disks")
    assert_equal(vols[1].raid_level, :JBOD, "Incorrect raid_level")
    assert_equal(vols[1].controller, a1_controller_5_disks[0], "Incorrect controller")
  end
  
  config_1_volume_cont2e = {
    "controllers"=> {
      "cntr1"=> { "raid_level_filter"=> [ "RAID1E" ] },
    },
    "volumes"=> {
      "0" => { "raid_level"=> "RAID1E" , "disk_count"=> "3", "order"=> 1, "controller"=> "cntr1" },
    }
  }

  test "13e" do
    vols, errs = build_volumes(config_1_volume_cont2e, a1_controller_no_disks)
    assert_equal(vols.length, 0, "volumes created")
    assert_equal(errs.length, 1, "Incorrect errors")
    assert_equal(errs[0], "Volume 0 doesn't match any controller", "Missing error")
  end

  test "14e" do
    vols, errs = build_volumes(config_1_volume_cont2e, a2_controller_no_disks)
    assert_equal(vols.length, 0, "volumes created")
    assert_equal(errs.length, 1, "Incorrect errors")
    assert_equal(errs[0], "Volume 0 doesn't match any controller", "Missing error")
  end

  test "15e" do
    vols, errs = build_volumes(config_1_volume_cont2e, a1_controller_3_disks)
    assert_equal(vols.length, 1, "volumes created")
    assert_equal(errs.length, 0, "Incorrect errors")
    assert_equal(vols[0].members.length, 3, "Incorrect disks")
    assert_equal(vols[0].raid_level, :RAID1E, "Incorrect raid_level")
  end

  test "16e" do
    vols, errs = build_volumes(config_1_volume_cont2e, a1_controller_5_disks)
    assert_equal(vols.length, 2, "volumes created")
    assert_equal(errs.length, 0, "Incorrect errors")
    assert_equal(vols[0].members.length, 3, "Incorrect disks")
    assert_equal(vols[0].raid_level, :RAID1E, "Incorrect raid_level")
    assert_equal(vols[0].controller, a1_controller_5_disks[0], "Incorrect controller")
    assert_equal(vols[1].members.length, 2, "Incorrect disks")
    assert_equal(vols[1].raid_level, :JBOD, "Incorrect raid_level")
    assert_equal(vols[1].controller, a1_controller_5_disks[0], "Incorrect controller")
  end
  
  config_1_volume_cont2er = {
    "controllers"=> {
      "cntr1"=> { "raid_level_filter"=> [ "RAID1E" ] },
    },
    "volumes"=> {
      "0" => { "raid_level"=> "RAID1E" , "disk_count"=> "remaining", "order"=> 1, "controller"=> "cntr1" },
    }
  }

  test "13er" do
    vols, errs = build_volumes(config_1_volume_cont2er, a1_controller_no_disks)
    assert_equal(vols.length, 0, "volumes created")
    assert_equal(errs.length, 1, "Incorrect errors")
    assert_equal(errs[0], "Volume 0 doesn't match any controller", "Missing error")
  end

  test "14er" do
    vols, errs = build_volumes(config_1_volume_cont2er, a2_controller_no_disks)
    assert_equal(vols.length, 0, "volumes created")
    assert_equal(errs.length, 1, "Incorrect errors")
    assert_equal(errs[0], "Volume 0 doesn't match any controller", "Missing error")
  end

  test "15er" do
    vols, errs = build_volumes(config_1_volume_cont2er, a1_controller_3_disks)
    assert_equal(vols.length, 1, "volumes created")
    assert_equal(errs.length, 0, "Incorrect errors")
    assert_equal(vols[0].members.length, 3, "Incorrect disks")
    assert_equal(vols[0].raid_level, :RAID1E, "Incorrect raid_level")
  end

  test "16er" do
    vols, errs = build_volumes(config_1_volume_cont2er, a1_controller_5_disks)
    assert_equal(vols.length, 1, "volumes created")
    assert_equal(errs.length, 0, "Incorrect errors")
    assert_equal(vols[0].members.length, 5, "Incorrect disks")
    assert_equal(vols[0].raid_level, :RAID1E, "Incorrect raid_level")
  end

  config_1_volume_cont3 = {
    "controllers"=> {
      "cntr1"=> { "raid_level_filter"=> [ "RAIDK" ] },
    },
    "volumes"=> {
      "0" => { "raid_level"=> "RAIDK" , "disk_count"=> "remaining", "order"=> 1, "controller"=> "cntr1" },
    }
  }

  test "17" do
    vols, errs = build_volumes(config_1_volume_cont3, a1_controller_no_disks)
    assert_equal(vols.length, 0, "volumes created")
    assert_equal(errs.length, 2, "Incorrect errors")
    assert_equal(errs[0], "Controller cntr1 has no options", "Missing error")
    assert_equal(errs[1], "Volume 0 doesn't have viable controller options", "Missing error")
  end

  test "18" do
    vols, errs = build_volumes(config_1_volume_cont3, a2_controller_no_disks)
    assert_equal(vols.length, 0, "volumes created")
    assert_equal(errs.length, 2, "Incorrect errors")
    assert_equal(errs[0], "Controller cntr1 has no options", "Missing error")
    assert_equal(errs[1], "Volume 0 doesn't have viable controller options", "Missing error")
  end

  test "19" do
    vols, errs = build_volumes(config_1_volume_cont3, a1_controller_3_disks)
    assert_equal(vols.length, 0, "volumes created")
    assert_equal(errs.length, 2, "Incorrect errors")
    assert_equal(errs[0], "Controller cntr1 has no options", "Missing error")
    assert_equal(errs[1], "Volume 0 doesn't have viable controller options", "Missing error")
  end

  test "20" do
    vols, errs = build_volumes(config_1_volume_cont3, a1_controller_5_disks)
    assert_equal(vols.length, 0, "volumes created")
    assert_equal(errs.length, 2, "Incorrect errors")
    assert_equal(errs[0], "Controller cntr1 has no options", "Missing error")
    assert_equal(errs[1], "Volume 0 doesn't have viable controller options", "Missing error")
  end

  test "21" do
    vols, errs = build_volumes(config_1_volume_cont3, a2_controller_5_disks)
    assert_equal(vols.length, 2, "volumes created")
    assert_equal(errs.length, 0, "Incorrect errors")
    assert_equal(vols[0].members.length, 5, "Incorrect disks")
    assert_equal(vols[0].raid_level, :RAIDK, "Incorrect raid_level")
    assert_equal(vols[0].controller, a2_controller_5_disks[1], "Incorrect controller")
    assert_equal(vols[1].members.length, 5, "Incorrect disks")
    assert_equal(vols[1].raid_level, :JBOD, "Incorrect raid_level")
    assert_equal(vols[1].controller, a2_controller_5_disks[0], "Incorrect controller")
  end

  config2 = {
    "volumes"=> {
      "0" => { "raid_level"=> "RAID10" , "disk_count"=> "4", "order"=> 1, "controller"=> "cntr1" },
      "1" => { "raid_level"=> "RAID10" , "disk_count"=> "remaining", "order"=> 2, "controller"=> "cntr1" }
    }
  }

  test "22" do
    vols, errs = build_volumes(config2, a2_controller_5_disks)
    assert_equal(vols.length, 0, "volumes created")
    assert_equal(errs.length, 1, "Incorrect errors")
    assert_equal(errs[0], "Volume 1 doesn't match any controller", "Missing error")
  end

  test "23" do
    vols, errs = build_volumes(config2, a2_controller_7_disks)
    assert_equal(vols.length, 4, "volumes created")
    assert_equal(errs.length, 0, "Incorrect errors")
    assert_equal(vols[0].members.length, 4, "Incorrect disks")
    assert_equal(vols[0].raid_level, :RAID10, "Incorrect raid_level")
    assert_equal(vols[0].controller, a2_controller_7_disks[1], "Incorrect controller")
    assert_equal(vols[1].members.length, 2, "Incorrect disks")
    assert_equal(vols[1].raid_level, :RAID1, "Incorrect raid_level")
    assert_equal(vols[1].controller, a2_controller_7_disks[1], "Incorrect controller")
    assert_equal(vols[2].members.length, 1, "Incorrect disks")
    assert_equal(vols[2].raid_level, :JBOD, "Incorrect raid_level")
    assert_equal(vols[2].controller, a2_controller_7_disks[1], "Incorrect controller")
    assert_equal(vols[3].members.length, 7, "Incorrect disks")
    assert_equal(vols[3].raid_level, :JBOD, "Incorrect raid_level")
    assert_equal(vols[3].controller, a2_controller_7_disks[0], "Incorrect controller")
  end

  test "24" do
    vols, errs = build_volumes(config2, a2_controller_3_disks)
    assert_equal(vols.length, 0, "volumes created")
    assert_equal(errs.length, 1, "Incorrect errors")
    assert_equal(errs[0], "Volume 1 doesn't match any controller", "Missing error")
  end

  config4 = {
    "controllers"=> {
      "cntr1"=> { }
    },
    "volumes"=> {
      "0" => { "raid_level"=> "RAID10" , "disk_count"=> "4", "order"=> 1, "controller"=> "cntr1" },
      "1" => { "raid_level"=> "RAID1E" , "disk_count"=> "remaining", "order"=> 2, "controller"=> "cntr1" }
    }
  }

  test "25" do
    vols, errs = build_volumes(config4, a2_controller_7_disks)
    assert_equal(vols.length, 0, "volumes created")
    assert_equal(errs.length, 1, "Incorrect errors")
    assert_equal(errs[0], "Volume 1 doesn't match any controller", "Missing error")
  end

  config5 = {
    "controllers"=> {
      "cntr1"=> { "device" => [ "Sleepy", "Dopey" ] }
    },
    "volumes"=> {
      "1" => { "raid_level"=> "RAID1E" , "disk_count"=> "remaining", "order"=> 2, "controller"=> "cntr1" }
    }
  }
  
  test "26" do
    vols, errs = build_volumes(config5, a2_controller_7_disks)
    assert_equal(vols.length, 0, "volumes created")
    assert_equal(errs.length, 2, "Incorrect errors")
    assert_equal(errs[0], "Controller cntr1 has no options", "Missing error")
    assert_equal(errs[1], "Volume 1 doesn't have viable controller options", "Missing error")
  end

  config5 = {
    "controllers"=> {
      "cntr1"=> { "device" => [ "John Prime", "Dopey" ] }
    },
    "volumes"=> {
      "1" => { "raid_level"=> "RAID1E" , "disk_count"=> "remaining", "order"=> 2, "controller"=> "cntr1" }
    }
  }
  
  test "27" do
    vols, errs = build_volumes(config5, a2_controller_7_disks)
    assert_equal(vols.length, 2, "volumes created")
    assert_equal(errs.length, 0, "Incorrect errors")
    assert_equal(vols[0].members.length, 7, "Incorrect disks")
    assert_equal(vols[0].raid_level, :RAID1E, "Incorrect raid_level")
    assert_equal(vols[0].controller, a2_controller_7_disks[0], "Incorrect controller")
    assert_equal(vols[1].members.length, 7, "Incorrect disks")
    assert_equal(vols[1].raid_level, :JBOD, "Incorrect raid_level")
    assert_equal(vols[1].controller, a2_controller_7_disks[1], "Incorrect controller")
  end

  config5 = {
    "controllers"=> {
      "cntr1"=> { "device" => [ "Fred Prime", "Dopey" ] }
    },
    "volumes"=> {
      "1" => { "raid_level"=> "RAIDK" , "disk_count"=> "remaining", "order"=> 2, "controller"=> "cntr1" }
    }
  }
  
  test "28" do
    vols, errs = build_volumes(config5, a2_controller_7_disks)
    assert_equal(vols.length, 2, "volumes created")
    assert_equal(errs.length, 0, "Incorrect errors")
    assert_equal(vols[0].members.length, 7, "Incorrect disks")
    assert_equal(vols[0].raid_level, :RAIDK, "Incorrect raid_level")
    assert_equal(vols[0].controller, a2_controller_7_disks[1], "Incorrect controller")
    assert_equal(vols[1].members.length, 7, "Incorrect disks")
    assert_equal(vols[1].raid_level, :JBOD, "Incorrect raid_level")
    assert_equal(vols[1].controller, a2_controller_7_disks[0], "Incorrect controller")
  end

  config6 = {
    "controllers"=> {
      "cntr1"=> { },
      "cntr2"=> { }
    },
    "volumes"=> {
      "0" => { "raid_level"=> "RAID10" , "disk_count"=> "4", "order"=> 1, "controller"=> "cntr1" },
      "1" => { "raid_level"=> "JBOD" , "disk_count"=> "remaining", "order"=> 2, "controller"=> "cntr2" }
    }
  }

  test "29" do
    vols, errs = build_volumes(config6, a1_controller_5_disks)
    assert_equal(vols.length, 0, "volumes created")
    assert_equal(errs.length, 1, "Incorrect errors")
    assert_equal(errs[0], "Volume 1 doesn't have viable controller options", "Missing error")
  end

  config3 = {
    "volumes"=> {
      "0" => { "raid_level"=> "RAID10" , "disk_count"=> "remaining", "order"=> 1, "controller"=> "cntr1" }
    }
  }

  config3a = {
    "volumes"=> {
      "0" => { "raid_level"=> "RAID1" , "disk_count"=> "2", "order"=> 1, "controller"=> "cntr1" },
      "1" => { "raid_level"=> "RAID10" , "disk_count"=> "remaining", "order"=> 2, "controller"=> "cntr1" }
    }
  }

  jbod_config = { 
    "volumes" => {
      "default" => { "raid_level" => "JBOD", "disk_count" => "remaining", "order"=> 3 }
    }
  }

end


if __FILE__ == $0
  $in_chef=true
#  load_current_resource
#  apply_config(config3, @controllers)
#  load_current_resource
#  do_set_boot(config3, @controllers, true)

#  load_current_resource
#  apply_config(config3a, @controllers)
#  load_current_resource
#  do_set_boot(config3a, @controllers, true)

=begin
The expressions below are some useful debugging tools when using IRB

-Given a bunch of volumes, display the volume-id/name (disk.enc:d.slot)-disk.vol_id
volumes.map { | x| s = x.members.map {|d| "#{d.enclosure}:#{d.slot}-#{d.vol_id}" }.sort.join(",");" #{x.raid_level} #{x.vol_id}/#{x.vol_name} :#{s}" } 

-Given a bunch of disks, dump some info
disks.map {|d| "#{d.enclosure}:#{d.slot}-#{d.vol_id}" }

- Simualte the discovery phase of the driver
@drivers, controllers = enumerate_topology

- build  a configuration (mapping disks to volumes on controllers)
config = {  "volumes" => {   "default" => { "raid_level"=> "JBOD", "disk_count"=> "remaining", "order"=> 3 } } }
volumes, errors = build_volumes(config, controllers)

- compute the differences between the desired and existing state.
v, rm_old,rm_new = get_volume_differences(volumes, controllers)
  

=end

end

  config1 = {
    "controllers"=> {
      "cntr1"=> { "raid_level_filter"=> [ "RAID5" ] },
      "cntr2"=> { "raid_level_filter"=> [ "RAID5" ] }
    },
    "volumes"=> {
      "0" => { "raid_level"=> "RAID5" , "disk_count"=> "6", "order"=> 1, "controller"=> "cntr1" },
      "1" => { "raid_level"=> "RAID5" , "disk_count"=> "6", "order"=> 2, "controller"=> "cntr1" },
      "2" => { "raid_level"=> "RAID5" , "disk_count"=> "6", "order"=> 3, "controller"=> "cntr2" },
      "3" => { "raid_level"=> "RAID5" , "disk_count"=> "6", "order"=> 4, "controller"=> "cntr2" }
    }
  }

  config1b = {
    "controllers"=> {
      "cntr1"=> { "raid_level_filter"=> [ "RAIDK" ] },
      "cntr2"=> { "raid_level_filter"=> [ "RAIDK" ] }
    },
    "volumes"=> {
      "0" => { "raid_level"=> "RAID5" , "disk_count"=> "6", "order"=> 1, "controller"=> "cntr1" },
      "1" => { "raid_level"=> "RAID5" , "disk_count"=> "6", "order"=> 2, "controller"=> "cntr1" },
      "2" => { "raid_level"=> "RAID5" , "disk_count"=> "6", "order"=> 3, "controller"=> "cntr2" },
      "3" => { "raid_level"=> "RAID5" , "disk_count"=> "6", "order"=> 4, "controller"=> "cntr2" }
    }
  }

