module BarclampRaid
  class Engine < ::Rails::Engine
    isolate_namespace BarclampRaid
    Dir.glob(File.join(File.dirname(File.expand_path(__FILE__)),"*.rb")).each do |f|
      require_relative File.join(".",File.basename(f,".rb"))
    end
  end
end
