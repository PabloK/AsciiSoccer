require './config/environment'
require './config/helpers'
require './lib/controllers'

map '/' do

  use Rack::Static, {
    :root => "public",
    :urls => ["/images", "/favicon.ico", "/robots.txt"],
    :cache_control => "public,max-age=#{365 * 24 * 3600}"
  }
  use Rack::Session::Cookie, :secret => ENV['SESSION_SECRETE'], :expire_after => 30 * 3600

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
    run SignupCotroller
  end

  map '/settings' do
    run SettingsCotroller
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
