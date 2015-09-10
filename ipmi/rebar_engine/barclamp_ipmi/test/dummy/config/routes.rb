Rails.application.routes.draw do

  mount BarclampIpmi::Engine => "/barclamp_ipmi"
end
