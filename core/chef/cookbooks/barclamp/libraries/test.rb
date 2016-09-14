
if $0 == __FILE__ 

  require 'test/unit'
  require 'barclamp_library'
  

  class TestIFRemap < Test::Unit::TestCase
    
    def build_if_map(speeds)
      if_map={}
      speeds.each {|x|
        if_map["#{x}1"]=x
      }
    if_map
    end
    
    def test_match_100m
      if_map=build_if_map %w{100m 1g}
      b = BarclampLibrary::Barclamp::Inventory
      assert_equal "100m", b.map_if_ref(if_map,"100m1")
    end
    
    def test_match_1g
      if_map=build_if_map %w{100m 1g}
    b = BarclampLibrary::Barclamp::Inventory
      assert_equal "1g", b.map_if_ref(if_map,"1g1")
    end
    
    def test_10m_upgrade
      if_map=build_if_map %w{100m 1g}
      b = BarclampLibrary::Barclamp::Inventory
      assert_equal "100m", b.map_if_ref(if_map,"+10m1")
    end
    
    def test_10gdowngrade
      b = BarclampLibrary::Barclamp::Inventory
      if_map=build_if_map %w{10m 100m 1g}
      assert_equal "1g", b.map_if_ref(if_map,"-10g1")
    end
    
    def test_1gdowngrade
      b = BarclampLibrary::Barclamp::Inventory
      if_map=build_if_map %w{10m 100m }
      assert_equal "100m", b.map_if_ref(if_map,"-1g1")
    end
    
    def test_1g_any
      b = BarclampLibrary::Barclamp::Inventory
      if_map=build_if_map %w{10m 100m }
      assert_equal "100m", b.map_if_ref(if_map,"?1g1")
    end
    
    def test_1g_any_up
      b = BarclampLibrary::Barclamp::Inventory
      if_map=build_if_map %w{10m 100m 10g}
      assert_equal "10g", b.map_if_ref(if_map,"?1g1")
    end

    def test_non_listed_items_with_one_valid
      b = BarclampLibrary::Barclamp::Inventory
      if_map=build_if_map %w{10m 100m 10g 0g 5k fred}
      assert_equal "10g", b.map_if_ref(if_map,"?1g1")
    end

    def test_non_listed_items_with_no_valid
      b = BarclampLibrary::Barclamp::Inventory
      if_map=build_if_map %w{0g 5k fred}
      assert_equal nil, b.map_if_ref(if_map,"?1g1")
    end
  end
end
