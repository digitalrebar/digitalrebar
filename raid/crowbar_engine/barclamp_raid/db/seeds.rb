source_path = File.expand_path(File.join(__FILE__,"../../../.."))
yml_blob = YAML.load_file(File.join(source_path,"crowbar.yml"))
Barclamp.import("raid",yml_blob,source_path)
