require './config/environment'
require './config/helpers'
require './lib/controllers'
require 'coffee-script'
require 'sass'

map '/' do

  use Rack::Session::Cookie, :secret => ENV['SESSION_SECRETE'], :expire_after => 30 * 3600

  cache_time = 0
  cache_time = 3600*24*7 if ENV['RACK_ENV'] == 'production'

  use Rack::Static, {
    :root => "public",
    :urls => ["/audio"],
    :content_type => "audio/mpeg",
    :cache_control => "public,max-age=#{cache_time}"
  }
  use Rack::Static, {
    :root => "public",
    :urls => ["/img"],
    :cache_control => "public,max-age=#{cache_time}"
  }
  use Rack::Static, {
    :root => "public",
    :urls => ["/vendor/js"],
    :content_type => "application/javascript",
    :cache_control => "public,max-age=#{cache_time}"
  }
  use Rack::Static, {
    :root => "public",
    :urls => ["/vendor/font"],
    :content_type => "application/x-font-woff",
    :cache_control => "public,max-age=#{cache_time}"
  }

  map '/css' do
    run SassCssConverter
  end

  map '/js' do
    run CoffeeJsConverter
  end

  map '/' do
    run PortalController
  end

  map '/log' do
    run LogController
  end

  map '/signup' do
    run SignupController
  end

  map '/settings' do
    run SettingsController
  end

  map '/recover' do
    run RecoverController
  end

  map '/game_queue' do
    run GameQueueController
  end

  map '/game' do
    run GameController
  end
end 
