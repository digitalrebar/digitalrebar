require 'rest-client'

class TrustedClient

  def initialize(url, username = 'system')
    store = OpenSSL::X509::Store.new
    store.add_cert(OpenSSL::X509::Certificate.new(File.read('/var/run/rebar/ca.pem')))
    
    # get client key and cert
    client_cert = OpenSSL::X509::Certificate.new(File.read('/var/run/rebar/server.crt'))
    client_key  = OpenSSL::PKey.read(File.read('/var/run/rebar/server.key'))
    caps = User.find_by!(username: username).cap_map.to_json
    @options = {
      :headers => {
        :content_type => :json,
        :accept => :json,
        'X-Authenticated-Username' => username,
        'X-Authenticated-Capability' => caps
      },
      :ssl_cert_store  =>  store,
      :ssl_client_cert =>  client_cert,
      :ssl_client_key  =>  client_key,
      :verify_ssl      =>  OpenSSL::SSL::VERIFY_PEER,
      :url => url
    }
  end

  def get
    exec(:get)
  end

  def put(data)
    exec(:put,data)
  end

  def post(data)
    exec(:post, data)
  end

  def patch(data)
    exec(:patch, data)
  end

  def delete
    exec(:delete)
  end

  private

  def exec(meth, data = nil)
    opts = @options.merge({:method => meth})
    opts = opts.merge({:payload => data}) if data
    RestClient::Request.execute(opts)
  end
  
end
