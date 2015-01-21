unless ENV['RAILS_ENV'] == 'test'
  begin
    require 'daemons'
  rescue LoadError
    raise "You need to add gem 'daemons' to your Gemfile if you wish to use it."
  end
end
require 'optparse'

module Delayed
  class HeartbeatCommand # rubocop:disable ClassLength
    attr_accessor :worker_count, :worker_pools

    def initialize(args) # rubocop:disable MethodLength
      @options = {
        :quiet => true,
        :pid_dir => "#{Rails.root}/tmp/pids"
      }

      @worker_count = 1
      @monitor = false

      opts = OptionParser.new do |opt|
        opt.banner = "Usage: #{File.basename($PROGRAM_NAME)} [options] start|stop|restart|run"

        opt.on('-h', '--help', 'Show this message') do
          puts opt
          exit 1
        end
        opt.on('-e', '--environment=NAME', 'Specifies the environment to run this delayed jobs under (test/development/production).') do |_e|
          STDERR.puts 'The -e/--environment option has been deprecated and has no effect. Use RAILS_ENV and see http://github.com/collectiveidea/delayed_job/issues/#issue/7'
        end
        opt.on('--pid-dir=DIR', 'Specifies an alternate directory in which to store the process ids.') do |dir|
          @options[:pid_dir] = dir
        end
        opt.on('-m', '--monitor', 'Start monitor process.') do
          @monitor = true
        end
      end
      @args = opts.parse!(args)
    end

    def daemonize # rubocop:disable PerceivedComplexity
      dir = @options[:pid_dir]
      Dir.mkdir(dir) unless File.exist?(dir)

      run_process("heartbeat.reaper", @options)
    end

    def run_process(process_name, options = {})
      Delayed::Worker.before_fork
      Daemons.run_proc(process_name, :dir => options[:pid_dir], :dir_mode => :normal, :monitor => @monitor, :ARGV => @args) do |*_args|
        while true do
          clear_workers
          sleep Rails.configuration.jobs.dead_worker_polling_interval_seconds
        end
      end
    end

    private

    def clear_workers
      timeout_seconds = Rails.configuration.jobs.heartbeat_timeout_seconds
      Delayed::Heartbeat::WorkerModel.dead_workers(timeout_seconds).delete_all
      orphaned_jobs = Delayed::Job.where("locked_at IS NOT NULL AND " \
                                         "locked_by NOT IN (#{Delayed::Heartbeat::WorkerModel.active_names.to_sql})")
      Rails.logger.info("Unlocking the following jobs: #{orphaned_jobs.inspect}")
      orphaned_jobs.update_all('locked_at = NULL, locked_by = NULL, attempts = attempts + 1')
    end

  end
end
