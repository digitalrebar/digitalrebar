#!/usr/bin/env ruby
def log(str)
  $stderr.puts(str)
end

unless File.exists?("sum_bios")
  log "Please run /path/to/sum -i example.system.ipmi.address -u user -p password -c GetDefaultBiosCfgTextTile --file sum_bios"
  log "This will get the default BIOS settings from the target SuperMicro system"
  log " For more information, read the manual for the SuperMicro Update Manager"
  exit 1
end
lines = IO.readlines("sum_bios")
header_re=/^\[(.+)\]$/
setting_re=/^([^=]+)=([0-9a-fA-F]+)/
default_val_re=/Default value is <<<([0-9a-fA-F]+)>>>/
header = ''
res = Hash.new
lines.each do |l|
  line = l.strip
  next if line.empty? || line[0] == '#'
  case
  when header_re =~ line
    header = $~[1]
  when setting_re =~ line
    name = header + "|" + $~[1]
    default = ""
    values = line.split("//",2)[1].split("        ",2)[0].strip.split('), ').map{|v|v.strip}
    if values.length == 1
      default = default_val_re.match(line)[1]
    else
      default = values.select{|v|v[0] == "*"}.first[1..-1]
      default << ")" if default[-1] != ")"
    end
    res[name] = default
  end
end

require 'json'
puts JSON.pretty_generate({"default" => res})
