namespace :db do
  namespace :fixtures do
    desc 'Create YAML test fixtures from data in an existing database.  Defaults to development database. Set RAILS_ENV to override.'
    task :dump => :environment do
      sql = "SELECT * FROM %s"
      skip_tables = ["schema_migrations"]
      dir = File.join(Rails.root, 'test', 'fixtures')
      FileUtils.mkdir_p dir
      ActiveRecord::Base.establish_connection(Rails.env)
      (ActiveRecord::Base.connection.tables - skip_tables).each do |table_name|
        i = "000"
        File.open(File.join(dir, "#{table_name}.yml"), 'w') do |file|
          data = ActiveRecord::Base.connection.select_all(sql % table_name)
          file.write data.inject({}) { |hash, record|
            hash["#{table_name}_#{i.succ!}"] = record
            hash
          }.to_yaml
        end
      end
    end
  end
end 
