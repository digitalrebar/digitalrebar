source_path = File.expand_path(File.join(__FILE__,"../../../.."))
yml_blob = YAML.load_file(File.join(source_path,"crowbar.yml"))
Barclamp.import("ipmi",yml_blob,source_path)
# Create some reverse dependencies to facilitate bootstrapping
RoleRequire.create!(role_id: Role.find_by!(name: "crowbar-admin-node").id, requires: "ipmi-master")
RoleRequire.create!(role_id: Role.find_by!(name: "crowbar-managed-node").id, requires: "ipmi-discover")
