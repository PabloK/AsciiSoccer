require 'rubygems'
require 'sinatra/base'
require 'sass'
require 'coffee-script'

class SassCssConverter < Sinatra::Base
    set :views, File.dirname(__FILE__) + "/../../assets/css"
    get '/*.css' do
      content_type 'text/css', :charset => 'utf-8'
      filename = params[:splat].first
      sass filename.to_sym
    end
end

class CoffeeJsConverter < Sinatra::Base
    set :views, File.dirname(__FILE__) + "/../../assets/js"
    get '/*.js' do
      content_type 'text/javascript', :charset => 'utf-8'
      filename = params[:splat].first
      coffee filename.to_sym
    end
end
