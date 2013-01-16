require 'rubygems'
require 'sinatra/base'
require 'data_mapper'

class  Sinatra::Base
  if ENV['RACK_ENV'] == 'production'
      set :haml, { :ugly=>true }
      set :clean_trace, true
  end

  set :views, File.dirname(__FILE__) + "/../views"
  
end

if ENV['RACK_ENV'] == 'development'
  DataMapper::Logger.new($stdout, :debug)
end
DataMapper::setup(:default, ENV['DATABASE_URL'])
