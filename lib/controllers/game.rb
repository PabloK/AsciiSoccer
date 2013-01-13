require 'rubygems'
require 'sinatra/base'
require 'haml'

class Game < Sinatra::Base
    get '/:port' do
        @port = params[:port]
        haml :index
    end
end
