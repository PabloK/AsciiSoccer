require 'sass'
require 'coffee-script'

class SassCssConverter < Sinatra::Base
    sass_dir= File.dirname(__FILE__) + "/../../assets/css"
    set :views, sass_dir
    get '/*.css' do
      content_type 'text/css', :charset => 'utf-8'
      filename = params[:splat].first
      sass filename.to_sym
    end
end

class CoffeeJsConverter < Sinatra::Base
    Tilt::CoffeeScriptTemplate.default_bare = true
    set :views, File.dirname(__FILE__) + "/../../assets/js"
    get '/*.js' do
      content_type 'text/javascript', :charset => 'utf-8'
      filename = params[:splat].first
      if ENV['RACK_ENV'] == 'production'
        Uglifier.compile(coffee filename.to_sym) 
      else
        coffee filename.to_sym
      end
    end
end
