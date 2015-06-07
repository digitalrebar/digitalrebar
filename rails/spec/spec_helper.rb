# SimpleCov supports only Ruby 1.9. It must be required and started before the
# application code loads, so keep this block at the top.
require 'simplecov'
SimpleCov.command_name 'RSpec'

# This file is copied to spec/ when you run 'rails generate rspec:install'
ENV["RAILS_ENV"] ||= 'test'
require File.expand_path("../../config/environment", __FILE__)
require 'rspec/rails'
require 'email_spec'

# Requires supporting ruby files with custom matchers and macros, etc,
# in spec/support/ and its subdirectories.
# (see https://github.com/rspec/rspec-core/issues/25 for the reason to use expand_path)
Dir[Rails.root.join("spec/support/**/*.rb")].each {|f| require File.expand_path(f) }

RSpec.configure do |config|
  # EMAIL test helpers
  config.include(EmailSpec::Helpers)
  config.include(EmailSpec::Matchers)

  # ## Mock Framework
  #
  # If you prefer to use mocha, flexmock or RR, uncomment the appropriate line:
  #
  # config.mock_with :mocha
  # config.mock_with :flexmock
  # config.mock_with :rr

  # Remove this line if you're not using ActiveRecord or ActiveRecord fixtures
  config.fixture_path = "#{::Rails.root}/spec/fixtures"

  # If you're not using ActiveRecord, or you'd prefer not to run each of your
  # examples within a transaction, remove the following line or assign false
  # instead of true.
  config.use_transactional_fixtures = true

  config.before(:suite) do
    DatabaseCleaner.strategy = :transaction
    DatabaseCleaner.clean_with(:truncation)
    Rails.application.load_seed # loading seeds
    source_path = File.realpath(File.join(Rails.root,".."))
    data = YAML.load_file(File.join(source_path,"crowbar.yml"))
    data['barclamp']['source_path'] = source_path
    Barclamp.import_or_update(data)
    Dir.glob(File.join(source_path,"barclamps","*.yml")).each do |yml|
      data = YAML.load_file(yml)
      data['barclamp']['source_path'] = source_path
      Barclamp.import_or_update(data)
    end
  end

  # If true, the base class of anonymous controllers will be inferred
  # automatically. This will be the default behavior in future versions of
  # rspec-rails.
  config.infer_base_class_for_anonymous_controllers = false

  # Run specs in random order to surface order dependencies. If you find an
  # order dependency and want to debug it, you can fix the order by providing
  # the seed, which is printed after each run.
  #     --seed 1234
  config.order = "random"

# Turn off for now.  May not need this.
  # Setup DatabaseCleaner helpers
#  config.before(:suite) do
#    DatabaseCleaner.strategy = :truncation
#  end
#  config.before(:each) do
#    DatabaseCleaner.start
#  end
#  config.after(:each) do
#    DatabaseCleaner.clean
#  end
end
