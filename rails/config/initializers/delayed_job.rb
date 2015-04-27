if Rails.env == "production"
  Delayed::Worker.logger = Logger.new("/var/log/crowbar/delayed_job.log")
else
  Delayed::Worker.logger = Logger.new(Rails.root.join("delayed_job.log"))
end
