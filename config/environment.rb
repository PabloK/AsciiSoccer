require 'rubygems'
require 'data_mapper'
require 'dm-core'
require 'dm-types'
require 'dm-serializer'
require 'dm-validations'
require 'dm-aggregates'
require 'dm-transactions'
require 'dm-constraints'
require 'sinatra/base'


if ENV['RACK_ENV'] == 'development'
  DataMapper::Logger.new($stdout, :debug)
end

if ENV['RACK_ENV'] == 'production'
  class Sinatra::Base
    set :haml, { :ugly=>true }
    set :clean_trace, true
  end
end

class Sinatra::Base
  set :views, File.dirname(__FILE__) + "/../views"
end

DataMapper.setup(:default, ENV['DATABASE_URL'])
