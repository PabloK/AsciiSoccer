#encoding: utf-8
# Show the current server environment on startup
if ENV['RACK_ENV']
  puts "[configuration] RACK_ENV : " + ENV['RACK_ENV']
end

if ENV['RACK_ENV'] == 'production'
  $config[:host] = ENV['ASCIISOCCER_SERVER_IP']
end

if ENV['RACK_ENV'] == 'development'
  $config[:host] = "ws://#{%x{hostname}.delete("\n")}:8080/"
end

class  Sinatra::Base
  if ENV['RACK_ENV'] == 'production'
    set :haml, { :ugly => true }
    set :clean_trace, true
  end
end

if ENV['RACK_ENV'] == 'development'
  DataMapper::Logger.new($stdout, :debug)
end
DataMapper::setup(:default, ENV['DATABASE_URL'])
