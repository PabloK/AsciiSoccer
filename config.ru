require './config/environment'
require './lib/controllers'

map '/' do

  use Rack::Static, {
    :root => "public",
    :urls => ["/images", "/favicon.ico", "/robots.txt"],
    :cache_control => "public,max-age=#{365 * 24 * 3600}"
  }
  use Rack::Session::Cookie, :secret => ENV['SESSION_SECRET'], :expire_after => 30 * 3600

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

  map '/recover' do
    run RecoverController
  end

  map '/game' do
    run GameController
  end
end 
