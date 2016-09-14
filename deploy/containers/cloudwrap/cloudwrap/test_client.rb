#!/usr/bin/env ruby

require 'jsonrpc-client'

ep=JSONRPC::Client.new("http://127.0.0.1:3030")

# Update for your use.  I've already changed this API key
ep_data = {
    "provider" => "AWS",
    "project_token" => "91SH1iTqwW5qpsDQbn1Z38Ma94gWh99c",
    "project_id" => "c8cfeca9-28d4-4cee-8280-8af2f5464f37",
    "debug" => {
      "host_ip" => "192.168.0.1",
      "boot_delay_time" => 300,
      "ssh_delay_time" => 30
    }
}

# List servers
servers = ep.invoke('servers.list', [ep_data])
p servers

# Register endpoint
#answer = ep.invoke('servers.register', [ep_data, 'root', []])
#p answer


