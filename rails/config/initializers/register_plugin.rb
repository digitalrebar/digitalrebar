require Rails.root.join('lib/delayed/heartbeat/plugin')
Delayed::Worker.plugins << Delayed::Heartbeat::Plugin
