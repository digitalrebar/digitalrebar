#!/usr/bin/ruby

require 'json'

class ::Hash
  def deep_merge(second)
    merger = proc { |key, v1, v2| Hash === v1 && Hash === v2 ? v1.merge(v2, &merger) : v2 }
    self.merge(second, &merger)
  end
end

answer = {}

records = Dir.glob("config/*.json")
records.each do |r|
  next if r == "config/final.json"
  next if r == "config/processed.json"
  b = JSON.parse( IO.read(r) )
  answer = answer.deep_merge(b)
end

answer["networks"] = []
networks = Dir.glob("config/networks/*.json")
networks.each do |n|
  net = JSON.parse( IO.read(n) )
  answer["networks"] << net
end

answer["services"] = []
services = Dir.glob("config/services/*.json").sort
services.each do |s|
  service = JSON.parse( IO.read(s) )
  answer["services"] << service
end

answer["filters"] = []
filteres = Dir.glob("config/filters/*.json")
filteres.each do |s|
  filter = JSON.parse( IO.read(s) )
  answer["filters"] << filter
end

answer["users"] = []
users = Dir.glob("config/users/*.json")
users.each do |u|
  user = JSON.parse( IO.read(u) )
  answer["users"] << user
end

answer["ssh_keys"] = {}
keys = Dir.glob("config/ssh_keys/*.key")
keys.each do |k|
  key_data = IO.readlines(k)
  if k =~ /config\/ssh_keys\/(.*).key/
    name = $1
  end
  answer["ssh_keys"][name] = key_data.join("")
end

puts JSON.pretty_generate(answer)
