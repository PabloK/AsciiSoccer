require 'rubygems'
require 'sinatra/base'
require 'sinatra/flash'
require 'data_mapper'

class  Sinatra::Base
  $config = {}
  if ENV['RACK_ENV'] == 'production'
      set :haml, { :ugly => true }
      set :clean_trace, true
  end

  if ENV['RACK_ENV'] == 'development'
    $config[:host] = "ws://pablo-N53SN:8080/"
  end
    
  colorfile = File.new(File.dirname(__FILE__) + "/../assets/css/color.sass","r")
  while line = colorfile.gets 
    line = line.delete(" $\n")
    name,value = line.split(":")
    $config[name.to_sym] = value
  end

  set :views, File.dirname(__FILE__) + "/../views"
  register Sinatra::Flash
  
end

if ENV['RACK_ENV'] == 'development'
  DataMapper::Logger.new($stdout, :debug)
end
DataMapper::setup(:default, ENV['DATABASE_URL'])
