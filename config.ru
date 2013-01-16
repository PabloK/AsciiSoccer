require './config/environment'
require './lib/controllers'

map '/' do

  map '/css' do
    run SassCssConverter
  end

  map '/js' do
    run CoffeeJsConverter
  end

  use Rack::Static, {
    :root => "public",
    :urls => ["/images", "/favicon.ico", "/robots.txt"],
    :cache_control => "public,max-age=#{365 * 24 * 3600}"
  }
  use Rack::Session::Cookie, :secret => ENV['SESSION_ SECRET'], :expire_after => 30 * 3600

  map '/' do
    run Portal
  end

  map '/game' do
    run Game
  end
end 
