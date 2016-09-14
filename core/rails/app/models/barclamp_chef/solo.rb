class BarclampChef::Solo < Role
  def sysdata(nr)
    { "rebar" =>{ "chef-solo" => {"name" => nr.node.name}}}
  end
end
