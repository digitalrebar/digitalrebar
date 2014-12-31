Gem::Specification.new do |s|
  s.name        = 'wsman'
  s.version     = '0.0.1'
  s.date        = '2012-03-19'
  s.summary     = "WS-MAN library"
  s.description = "WS-MAN library"
  s.authors     = ["David Paterson"]
  s.email       = 'david_paterson@dell.com'
  s.files       = ["lib/wsman.rb"]
  s.homepage    = 'http://dell.com/wsman'
  s.add_dependency('xml-simple', '>= 1.0.0')
end