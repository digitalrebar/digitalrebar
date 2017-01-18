res = Hash.new
lines = IO.readlines("sum_bios")
header_re=/^\[(.+)\]$/
setting_re=/^([^=]+)=([0-9a-fA-F]+)/
option_re=/\*?(([0-9a-fA-F]+) \(.+)/
default_enum_re=/\*(([0-9a-fA-F]+) \(.+)/
default_val_re=/Default value is <<<([0-9a-fA-F]+)>>>/
header = ''
lines.each do |l|
  line = l.strip
  next if line.empty? || line[0] == '#'
  i = Hash.new
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
    puts "\"#{name}\": \"#{default}\""
  end
end