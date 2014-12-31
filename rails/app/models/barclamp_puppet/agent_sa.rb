class BarclampPuppet::AgentSa < Role
  def sysdata(nr)
    Rails.logger.info("Puppet agent node: #{nr.node.name}")
    { "crowbar" =>{ "puppet-agent-sa" => {"name" => nr.node.name}}}
  end
end
