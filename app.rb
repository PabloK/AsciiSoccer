require './config/environment'
require './lib/controllers'

class Sinatra::Base
  set :views, File.dirname(__FILE__) + "/views"
end
 
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

  map '/' do
    run Game
  end
end 
