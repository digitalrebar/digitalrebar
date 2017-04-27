#!/usr/bin/env ruby

require "yaml"
require "fileutils"

src=ARGV[0]
dest=ARGV[1]

if src.nil?
  STDERR.puts("No source .yml")
  exit 1
end

if dest.nil?
  STDERR.puts("No destination")
  exit 1
end

if !File.exists?(File.join(dest,"BOOK.rst"))
  STDERR.puts("Dest not at top of documentation tree")
  exit 1
end

def write_doc(path, name, description, documentation)
  File.open(File.join(path,name + ".rst"),"w") do |f|
    description ||= "No Description"
    documentation ||= "No Documentation"
    f.puts("=" * name.length,name,"=" * name.length)
    f.puts
    f.puts("Description")
    f.puts("===========")
    f.puts(description)
    f.puts
    f.puts("Documentation")
    f.puts("=============")
    f.puts
    f.puts(documentation)
  end
end

rebar_yml = YAML.load_file(src)

attrpath=File.join(dest,"ref","attribs")
rolepath=File.join(dest,"ref","roles")
FileUtils.mkdir_p(attrpath)
FileUtils.mkdir_p(rolepath)
rebar_yml["attribs"].each do |a|
  write_doc(attrpath,a["name"],a["description"],a["documentation"])
end if rebar_yml["attribs"]

rebar_yml["roles"].each do |r|
  write_doc(rolepath,r["name"],r["description"],r["documentation"])
  r["attribs"].each do |a|
    write_doc(attrpath,a["name"],a["description"],a["documentation"])
  end if r["attribs"]
end if rebar_yml["roles"]
