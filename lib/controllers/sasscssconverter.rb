require 'rubygems'
require 'sinatra/base'
require 'sass'

class SassCssConverter < sinatra::base
    get '/*.css' do
      content_type 'text/css', :charset => 'utf-8'
      filename = params[:splat].first
      puts filename
      sass filename.to_sym, :views => "#{settings.root}/assets/stylesheets"
    end
end
