require 'rubygems'
require 'em-websocket'
require 'sinatra/base'
require 'haml'
require 'socket'

class App < Sinatra::Base
    get '/' do
        haml :index
    end
end
