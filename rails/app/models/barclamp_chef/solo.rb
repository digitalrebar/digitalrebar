class BarclampChef::Solo < Role
  def sysdata(nr)
    { "crowbar" =>{ "chef-solo" => {"name" => nr.node.name}}}
  end
end
