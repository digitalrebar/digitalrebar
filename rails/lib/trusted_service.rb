class TrustedService

  def self.url(name)
    svc = ConsulAccess.getService(name,:first, tag: "revproxy")
    return nil if svc.nil?
    addr = (svc.ServiceAddress.nil? || svc.ServiceAddress.empty?) ? svc.Address : svc.ServiceAddress 
    port = svc.ServicePort
    "https://#{addr}:#{port.nil? || port == 0 ? 443 : port}"
  end

end
