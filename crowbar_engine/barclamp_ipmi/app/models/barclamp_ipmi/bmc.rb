class BarclampIpmi::Bmc < BarclampNetwork::Role

  def range_name(nr)
    "bmc"
  end

  def on_proposed(nr)
    NodeRole.transaction do
      if network.allocations.node(nr.node).count == 0
        # we need to create a bmc network
        Rails.logger.info("Creating BMC network for #{nr.node.name}")
      end
      super nr
    end
  end

end