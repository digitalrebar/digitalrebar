require 'rubygems'
require 'sinatra'
require 'sinatra/partial'
require 'json'

configure do 
  set :port, 3000
  
  register Sinatra::Partial
  set :partial_template_engine, :erb
  enable :partial_underscores
end

post '/save/:network' do
  begin
    request.body.rewind
    data = JSON.pretty_generate JSON.parse request.body.read
    open("../../config/networks/#{params['network']}.json","wb+") do |file|
      file.write data
    end
  ensure
    redirect '/'
  end
end


get '/' do
  @networks = Dir['../../config/networks/*'].map do |f|
    JSON.parse(open(f).read) rescue nil
  end

  @networks.compact!
  erb :index
end