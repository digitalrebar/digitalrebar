#!/usr/bin/env ruby

require 'jsonrpc-client'

ep=JSONRPC::Client.new("http://127.0.0.1:3031")

# Update for your use.  I've already changed this API key
ep_data = {
  "project_id"=>"c8cfeca9-28d4-4cee-8280-8af2f5464f37",
  "project_token"=>"PPu4tJiKKa5LKnxGcQRXuQqxvzhFQnu1"
}

# List servers
#servers = ep.invoke('servers.list', [ep_data])
#p servers

# Register endpoint
answer = ep.invoke('servers.register', [ep_data, 'root', []])
p answer


