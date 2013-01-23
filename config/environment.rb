require 'rubygems'
require 'sinatra/base'
require 'sinatra/flash'
require 'data_mapper'
require 'pony'

class  Sinatra::Base
  $config = {}
  if ENV['RACK_ENV'] == 'production'
      set :haml, { :ugly => true }
      set :clean_trace, true
  end

  if ENV['RACK_ENV'] == 'development'
    $config[:host] = "ws://#{%x{hostname}.delete("\n")}:8080/"
  end

  $config[:first_port] = 4000
  $config[:last_port] = 4100

  colorfile = File.new(File.dirname(__FILE__) + "/../assets/css/color.sass","r")
  while line = colorfile.gets 
    line = line.delete(" $\n")
    name,value = line.split(":")
    $config[name.to_sym] = value
  end

  set :views, File.dirname(__FILE__) + "/../views"
  register Sinatra::Flash

    options = {      
    :from => "noreply@asciisoccer.herokuapps.com",
    :via => :smtp,
    :via_options => {
        :user_name => ENV['SENDGRID_USERNAME'],
        :password => ENV['SENDGRID_PASSWORD'],
        :domain => "localhost",
        :address => "smtp.sendgrid.net",
        :port => 587,
        :authentication => :plain,
        :enable_starttls_auto => true
      }
    }
  Pony.options = options
  
end

if ENV['RACK_ENV'] == 'development'
  DataMapper::Logger.new($stdout, :debug)
end
DataMapper::setup(:default, ENV['DATABASE_URL'])
