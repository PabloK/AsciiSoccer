require 'haml'

class GameController < Sinatra::Base
    get '/:port' do
        @port = params[:port]
        haml :game, :layout => false
    end
end
