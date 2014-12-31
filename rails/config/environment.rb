# Load the rails application
require File.expand_path('../application', __FILE__)

if File.exists?("../chef/cookbooks/barclamp/libraries")
  require "../chef/cookbooks/barclamp/libraries/ip.rb"
  require "../chef/cookbooks/barclamp/libraries/nic.rb"
  require "../chef/cookbooks/barclamp/libraries/nethelper.rb"
end

# Initialize the rails application
Crowbar::Application.initialize!
