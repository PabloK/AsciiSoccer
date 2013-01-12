require 'rubygems'
require 'sinatra/base'
require 'haml'

class App < Sinatra::Base
    get '/' do
        haml :index
    end
    get '/:port' do
        port = params[:port]
        haml :index
    end
end
